{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.Tutorial.Console where

import Control.Applicative
import Control.Concurrent
import qualified Control.Concurrent.Async.Lifted.Safe as A
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar.Extras as STE
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Data.Constraint.Forall (Forall)
import qualified Data.Decimal as D
import Data.Foldable
import Data.List (intersperse)
import qualified Data.Map.Monoidal.Strict as MM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T
import qualified Glazier as G
import qualified Glazier.Pipes.Ui as GP
import qualified Glazier.Tutorial.App as GTA
import qualified Glazier.Tutorial.Counter as GTC
import qualified Glazier.Tutorial.Field as GTF
import qualified Glazier.Tutorial.Random as GTR
import qualified Glazier.Tutorial.StreamModel as GTS
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Fluid as PF
import qualified Pipes.Lift as PL
import qualified Pipes.Misc.Concurrent as PM
import qualified Pipes.Misc.State.Strict as PM
import qualified Pipes.Misc.Time as PM
import qualified Pipes.Misc.Util as PM
import qualified Pipes.Prelude as PP
import qualified System.Clock as C
import qualified System.Console.ANSI as ANSI
import qualified System.IO as IO

data StreamConfig = StreamConfig
    { streamConfigStreamInput1 :: PC.Input D.Decimal
    , streamConfigStreamInput2 :: PC.Input D.Decimal
    }
makeFields ''StreamConfig

-- | Widget update command
data AppCommand
    = AppCounterCommand GTC.CounterCommand
    | AppActionCommand GTA.AppAction
    | QuitCommand

-- | Rendering instruction
-- NB. Free monad is not required for this interpreter
-- * chaining computations are done using Free Monoid of List by the Glazier framework.
-- * no need for monadic control flow in rendering.
data Rendering
    = DisplayText T.Text

type Frontend ctl rndr = (MM.MonoidalMap Char [ctl], [rndr])

counterWindow :: Monad m => T.Text -> G.WindowT GTC.CounterModel m (Frontend ctrl Rendering)
counterWindow txt = do
    n <- ask
    pure (mempty, [DisplayText . T.append txt . T.pack . show $ n])

counterButtonWindow
  :: Monad m => (GTC.CounterAction -> ctl) -> Char -> T.Text -> GTC.CounterAction -> G.WindowT GTC.CounterModel m (Frontend ctl Rendering)
counterButtonWindow sendAction c txt action = do
    n <- ask
    pure (view (from _Wrapped') $ M.singleton c [ctl n], [render txt])
  where
    render = DisplayText
    -- NB. Although it's possible to have different control behaviour based on the state
    -- This is not good practice because it could get out of sync if processing becomes busy.
    -- Eg. Say that the control increments in larger amounts as the count becomes larger.
    -- If processing was held up, and there was a backlog of decrements.
    -- all the decrements fired will be large decrements, instead of slowly smaller decrements.
    -- It is much safer to have stateful logic in the `Gadget`, instead of the `Window`.
    ctl = const $ sendAction action

fieldWindow :: Monad m => (a -> T.Text) -> G.WindowT a m (Frontend ctrl Rendering)
fieldWindow f = do
    msg <- ask
    pure ( mempty --ctls
         , let msg' = f msg
             in if T.null $ msg' -- render
                then []
                else pure . DisplayText $ msg'
         )

quitWidget :: Monad m => (GTA.AppAction -> ctl) -> (G.WindowT GTA.AppModel m (Frontend ctl Rendering),
                                                    G.GadgetT GTA.AppAction GTA.AppModel m [AppCommand])
quitWidget sendAction = (w, g)
  where
    w = pure ( MM.MonoidalMap $ M.singleton 'q' [sendAction GTA.QuittingAction]
             , [DisplayText "Press 'q' to quit"])
    g = G.GadgetT $ do
             a <- ask
             lift $
                 case a of
                     GTA.QuittingAction -> do
                         GTA.messageModel .= "Quitting"
                         pure [AppActionCommand GTA.QuitAction]
                     GTA.QuitAction -> pure [QuitCommand] -- quit immediately
                     _ -> pure []

messageWidget ::
  (GTA.HasMessageModel s T.Text, GTA.AsAppAction a, Monad m) =>
  (G.WindowT s m (MM.MonoidalMap Char [ctrl], [Rendering]),
   G.GadgetT a s m [c])
messageWidget =  G.implant GTA.messageModel $ G.dispatch (GTA._AppMessageAction . GTF._FieldAction) $ G.Widget
    (fieldWindow (\s -> (T.append "Message: " s)))
    GTF.fieldGadget

spaceWindow ::
  (Monoid t, Applicative m) => G.WindowT s m (t, [Rendering])
spaceWindow = pure (mempty, [DisplayText " "])

newlineWindow ::
  (Monoid t, Monad m) => G.WindowT s m (t, [Rendering])
newlineWindow = pure (mempty, [DisplayText "\n"])

counterWidget ::
  (GTA.HasCounterModel s Int, GTC.AsCounterAction a, Monad m) =>
  (GTC.CounterAction -> ctl)
  -> G.Widget
       m (MM.MonoidalMap Char [ctl], [Rendering]) a s m [GTC.CounterCommand]
counterWidget sendAction' = G.implant GTA.counterModel $ G.dispatch GTC._CounterAction $
   G.Widget
    -- NB. Don't have a counterButtonGadget per buttonWindow - that will mean
    -- an inc/dec action will be evaluated twice!
    -- Ie. consider making update idempotent to avoid manually worrrying about this problem.
    -- Alternatively, have an incrementGadget and decrementGadget.
    (fold <$> (sequenceA $ intersperse spaceWindow
        [ counterButtonWindow sendAction' '+' "Press '+' to increment." GTC.Increment
        , counterButtonWindow sendAction' '-' "Press '-' to decrement." GTC.Decrement
        ]))
    (GTC.counterButtonGadget 5000)

counterWidget' ::
  (GTA.HasCounterModel s Int, GTC.AsCounterAction a, Monad m) =>
  (GTC.CounterAction -> ctl)
  -> G.Widget
       m (MM.MonoidalMap Char [ctl], [Rendering]) a s m [AppCommand]
counterWidget' sendAction' = fmap AppCounterCommand <$> (counterWidget sendAction')

menuWidget ::
  Monad m =>
  (GTA.AppAction -> ctl)
  -> G.Widget
       m
       (MM.MonoidalMap Char [ctl], [Rendering])
       GTA.AppAction
       GTA.AppModel
       m
       [AppCommand]
menuWidget sendAction = foldMap id $ intersperse (G.statically spaceWindow) [counterWidget' sendAction', quitWidget sendAction]
  where sendAction' = sendAction . GTA.AppCounterAction

counterDisplayWidget ::
  (GTA.HasCounterModel s Int, Monoid c, Monad m) =>
  G.Widget m (MM.MonoidalMap Char [ctrl], [Rendering]) a s m c
counterDisplayWidget = G.implant GTA.counterModel $ G.statically $ counterWindow "Current count is: "

signal1Window ::
  (GTS.HasSignal1 s (Maybe D.Decimal), Monad m) =>
  G.WindowT s m (Frontend ctrl Rendering)
signal1Window = fieldWindow $ \s -> "Signal1: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal1)

signal2Window ::
  (GTS.HasSignal2 s (Maybe D.Decimal), Monad m) =>
  G.WindowT s m (Frontend ctrl Rendering)
signal2Window = fieldWindow $ \s -> "Signal2: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal2)

ratioWindow ::
  (GTS.HasRatioOfSignals s [D.Decimal], Monad m) =>
  G.WindowT s m (Frontend ctrl Rendering)
ratioWindow = fieldWindow $ \s -> "Ratio: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^? (GTS.ratioOfSignals . ix 0))

ratioThresholdCrossedWindow ::
  (GTS.HasRatioThresholdCrossed s a, Monad m, Show a) =>
  G.WindowT s m (Frontend ctrl Rendering)
ratioThresholdCrossedWindow = fieldWindow $ \s -> "Crossed?: " `T.append` (T.pack . show $ s ^. GTS.ratioThresholdCrossed)

ratioThresholdCrossedPinWindow ::
  (GTS.HasRatioThresholdCrossedPin s D.Decimal, Monad m) =>
  G.WindowT s m (Frontend ctrl Rendering)
ratioThresholdCrossedPinWindow = fieldWindow $ \s -> "CrossedPin: " `T.append` (T.pack . show $ s ^. GTS.ratioThresholdCrossedPin)

signalsWindow ::
  (GTS.HasRatioOfSignals s [D.Decimal],
   GTS.HasRatioThresholdCrossed s a,
   GTS.HasRatioThresholdCrossedPin s D.Decimal,
   GTS.HasSignal1 s (Maybe D.Decimal),
   GTS.HasSignal2 s (Maybe D.Decimal), Monad m,
   Show a) =>
  G.WindowT s m (MM.MonoidalMap Char [ctrl], [Rendering])
signalsWindow = foldMap id $ intersperse newlineWindow [signal1Window, signal2Window, ratioWindow, ratioThresholdCrossedWindow, ratioThresholdCrossedPinWindow]

signalsWidget ::
  (GTA.HasStreamModel s GTS.StreamModel, GTA.AsAppAction a,
   Monad m) =>
  G.Widget m (MM.MonoidalMap Char [ctrl], [Rendering]) a s m [c]
signalsWidget = G.implant GTA.streamModel $ G.dispatch (GTA._SetStreamModel . GTF._FieldAction) $ G.Widget signalsWindow GTF.fieldGadget

appWidget :: Monad m => (GTA.AppAction -> ctl) -> G.Widget m (Frontend ctl Rendering) GTA.AppAction GTA.AppModel m [AppCommand]
appWidget sendAction = foldMap id $
    intersperse (G.statically newlineWindow)
    [ G.statically newlineWindow
    , messageWidget
    , counterDisplayWidget
    , signalsWidget
    , menuWidget sendAction
    , G.statically newlineWindow
    ]

renderFrame :: MonadIO io => [Rendering] -> io ()
renderFrame frame = do
    liftIO ANSI.clearScreen
    liftIO $ traverse_ process frame
  where
    process (DisplayText txt) = putStr . T.unpack $ txt

-- | Get user input and pump into address AppAction
interpretControls :: MonadIO io =>
  (GTA.AppAction -> MaybeT STM ())
  -> TMVar (MM.MonoidalMap Char [MaybeT STM ()])
  -> MaybeT io ()
interpretControls sendAction ctls = do
    c <- liftIO getChar
    hoist (liftIO . atomically) $ do
        ctls' <- lift $ readTMVar ctls
        -- lookup what STM actions to do for that key pressed
        case M.lookup c (view _Wrapped' ctls') of
            Nothing ->
                sendAction
                    (GTA.AppMessageAction
                         (GTF.ModifyField $ const "Invalid user input"))
            -- reset message on user input
            Just xs -> do
                sendAction (GTA.AppMessageAction (GTF.ModifyField $ const ""))
                sequenceA_ xs

withNoBuffering :: IO a -> IO a
withNoBuffering action =
    bracket (IO.hGetBuffering IO.stdin) (IO.hSetBuffering IO.stdin) $ \_ -> do
        IO.hSetBuffering IO.stdin IO.NoBuffering
        action

-- external effect processing
interpretCommand
    :: (MonadIO io)
    =>   (GTA.AppAction -> MaybeT io ()) -> AppCommand -> MaybeT io ()
interpretCommand sendAction = process
  where
    process QuitCommand = empty
    process (AppActionCommand a) = sendAction a
    process (AppCounterCommand c) = liftIO $ print c

renderFrame' ::
  MonadIO m =>
  (GTA.AppAction -> ctl)
  -> TMVar (MM.MonoidalMap Char [ctl]) -> GTA.AppModel -> m ()
renderFrame' sendAction ctls s = do
    (ctls', frame') <- view G._WindowT (G.window $ appWidget sendAction) s
    liftIO . atomically . void $ STE.forceSwapTMVar ctls ctls'
    liftIO $ renderFrame frame'

-- signal network
mkSignal ::
  (MonadState s (t STM), MonadTrans t) =>
  Lens' s (Maybe a) -> PC.Input a -> P.Producer a (t STM) ()
mkSignal lns input = hoist lift (PM.fromInputSTM input) P.>-> PM.store (to Just) lns

ratioSignal
    :: ( HasStreamInput1 cfg (PC.Input a)
       , HasStreamInput2 cfg (PC.Input a)
       , GTS.HasSignal1 s (Maybe a)
       , GTS.HasSignal2 s (Maybe a)
       , MonadState s (t STM)
       , MonadTrans t
       , Alternative (t STM)
       , Fractional a
       )
    => cfg -> P.Producer a (t STM) ()
ratioSignal c = PF.impulsively $ (/) <$> PF.Impulse (mkSignal GTS.signal1 (c ^. streamInput1)) <*> PF.Impulse (mkSignal GTS.signal2 (c ^. streamInput2))

ratiosSignal' ::
  (HasStreamInput1 cfg (PC.Input b),
   HasStreamInput2 cfg (PC.Input b), GTS.HasRatioOfSignals s [b],
   GTS.HasSignal1 s (Maybe b), GTS.HasSignal2 s (Maybe b),
   MonadState s (t STM), MonadTrans t, Alternative (t STM),
   Fractional b) =>
  cfg -> P.Producer [b] (t STM) ()
ratiosSignal' c = ratioSignal c P.>-> PM.buffer 2 [] P.>-> PM.store id GTS.ratioOfSignals

-- thresholdCrossedSignal2 :: P.Pipe [D.Decimal] (Maybe GTS.CrossedDirection) (StateT s STM) ()
thresholdCrossedPipe ::
  (GTA.HasThreshold s D.Decimal, MonadState s m) =>
  P.Pipe [D.Decimal] (Maybe GTS.CrossedDirection) m ()
thresholdCrossedPipe = P.for P.cat $ \rs -> do
  t <- lift $ use GTA.threshold
  let r = rs ^? ix 0
      prevR = rs ^? ix 1
  P.yield (GTS.thresholdCrossed (Just t) r prevR)

thresholdCrossedSignal' ::
  (GTA.HasThreshold s D.Decimal,
   HasStreamInput1 cfg (PC.Input D.Decimal),
   HasStreamInput2 cfg (PC.Input D.Decimal),
   GTS.HasRatioOfSignals s [D.Decimal],
   GTS.HasRatioThresholdCrossed s (Maybe GTS.CrossedDirection),
   GTS.HasRatioThresholdCrossedPin s D.Decimal,
   GTS.HasSignal1 s (Maybe D.Decimal),
   GTS.HasSignal2 s (Maybe D.Decimal), MonadState s (t STM),
   MonadTrans t, Alternative (t STM)) =>
  cfg -> P.Producer (Maybe GTS.CrossedDirection) (t STM) ()
thresholdCrossedSignal' c = ratiosSignal' c P.>-> thresholdCrossedPipe
    -- only set ratioThresholdCrossed to Nothing iff
    -- ratioThresholdCrossedPin is 0
    P.>-> P.for P.cat go
  where
    go a = do
        if isNothing a
           then do
               p <- use GTS.ratioThresholdCrossedPin
               if p <= D.Decimal 0 0
                  then GTS.ratioThresholdCrossed .= a -- Nothing
                  else pure ()
           else do
               GTS.ratioThresholdCrossed .= a
               GTS.ratioThresholdCrossedPin .= D.Decimal 0 1
        P.yield a

-- | convert the stream of delta time, to a stream of pin state changes due to the time
animatePin :: (MonadState s (t STM), GTS.HasRatioThresholdCrossedPin s D.Decimal) => P.Pipe C.TimeSpec C.TimeSpec (t STM) r
animatePin = P.for P.cat $ \d -> do
    GTS.ratioThresholdCrossedPin %= (\p ->
                    if p > D.Decimal 0 0
                       then max (D.Decimal 0 0) (p - D.Decimal 9 (C.toNanoSecs d))
                       else p)
    P.yield d

-- | This is similar to part of the Elm startApp.
-- This is responsible for setting up the external signal network before running
-- the glazier widget framework using 'runUi'.
exampleApp ::
  (MonadBaseControl IO io, Forall (A.Pure io),
   MonadIO io) =>
  Int
  -> Int
  -> D.Decimal
  -> D.Decimal
  -> D.Decimal
  -> D.Decimal
  -> (Int, Int)
  -> D.Decimal
  -> D.Decimal
  -> D.Decimal
  -> D.Decimal
  -> (Int, Int)
  -> GTA.AppModel
  -> io ()
exampleApp
  animationDelay
  refreshDelay
  start1
  tick1
  desired1
  threshold1
  interval1
  start2
  tick2
  desired2
  threshold2
  interval2
  appModel
  = do
    -- threads for generating random signals
    (output1, input1) <- liftIO . PC.spawn $ PC.newest 1
    void . liftIO . forkIO . void . (`execStateT` start1) . forever . runMaybeT $ GTR.randomSignal
        tick1
        desired1
        threshold1
        interval1
        (PC.send output1)

    (output2, input2) <- liftIO . PC.spawn $ PC.newest 1
    void . liftIO . forkIO . void . (`execStateT` start2) . forever . runMaybeT $ GTR.randomSignal
        tick2
        desired2
        threshold2
        interval2
        (PC.send output2)

    -- threads for rendering and controlling UI
    (outputUi, inputUi) <- liftIO $ PC.spawn PC.unbounded
    let sendAction = PM.toOutputMaybeT outputUi
        sendActionIO = hoist (liftIO . atomically) . sendAction

    -- controls thread
    -- continuously process user input using ctls until it fails (quit)
    -- pushs actions into update thread
    ctls <- liftIO newEmptyTMVarIO
    ctlsThread <- liftIO . forkIO . void . withNoBuffering . runMaybeT . forever $
        interpretControls sendAction ctls

    -- ticker
    let timer = PM.always () P.>-> PM.delay' animationDelay P.>-> PM.ticker C.Monotonic P.>-> PM.diffTime
    timerSTM <- liftIO $ PM.mkProducerSTM (PC.bounded 100) timer
    let animation = hoist lift timerSTM P.>-> animatePin

    -- combine the stream signal and animation effects
    let streamImpulse = (\_ _ -> ()) <$> PF.Impulse (thresholdCrossedSignal' (StreamConfig input1 input2)) <*> PF.Impulse animation
        gadgetImpulse = PF.Impulse $ PM.rsProducer inputUi (G.runGadgetT . G.gadget $ appWidget sendAction)
        -- appSignalIO :: P.Producer [AppCommand] (StateT GTA.AppModel STM) ()
        -- combine the stream effects with the gadget signal, keeping only the yields from gadget signal
        appSignal = PF.impulsively $ fromMaybe mempty . PF.discreteLeft <$> (gadgetImpulse `PF.merge` streamImpulse)

        -- combine the app signal with interpreting the command output
        -- appSignalIO :: P.Producer [AppCommand] (MaybeT (StateT GTA.AppModel io)) ()
        appSignalIO = hoist (lift . hoist (liftIO . atomically)) appSignal
        -- interpretCommandsPipe :: P.Pipe [AppCommand] () (MaybeT (StateT GTA.AppModel io)) ()
        interpretCommandsPipe = PP.mapM (hoist lift . traverse_ (interpretCommand sendActionIO))

        -- run the MaybeT (StateT) in appSignal and into just MonadIO
        -- and convert to a Producer of AppModel
        -- appSignalIO' :: P.Producer GTA.AppModel io GTA.AppModel
        appSignalIO' =
            PL.execStateP appModel $
            PL.runMaybeP $
            appSignalIO P.>-> interpretCommandsPipe P.>-> PM.retrieve' id

    -- finally run the main gui threads
    s <- GP.runUi refreshDelay (renderFrame' sendAction ctls) appSignalIO'
    liftIO $ killThread ctlsThread
    liftIO $ print s
