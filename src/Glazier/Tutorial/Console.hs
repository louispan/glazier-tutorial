{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Glazier.Tutorial.Console where

-- import Control.Monad.Trans.Identity
import Control.Applicative
import Control.Concurrent
-- import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar.Extras as STE
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
-- import qualified Control.Monad.Trans.State.Strict.Extras as SE
-- import qualified Control.Monad.Trans.Reader.Extras as RE
import qualified Data.Decimal as D
import Data.Foldable
import Data.List (intersperse)
import qualified Data.Map.Monoidal.Strict as MM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Glazier as G
import qualified Glazier.Strict as G
import qualified Glazier.Pipes.Strict as GP
import qualified Glazier.Tutorial.App as GTA
import qualified Glazier.Tutorial.Counter as GTC
import qualified Glazier.Tutorial.Field as GTF
import qualified Glazier.Tutorial.Random as GTR
import qualified Glazier.Tutorial.StreamModel as GTS
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Fluid.React as PFR
import qualified Pipes.Lift as PL
import qualified Pipes.Misc.Concurrent as PM
import qualified Pipes.Misc.State.Strict as PM
import qualified Pipes.Misc.Util as PM
import qualified Pipes.Prelude as PP
-- import qualified Pipes.Prelude as PP
import qualified System.Console.ANSI as ANSI
import qualified System.IO as IO
-- import qualified System.Mem as SM
import qualified Glazier.Pipes.Stopwatch as GPS

data StreamConfig = StreamConfig
    { streamConfigStreamInput1 :: PC.Input D.Decimal
    , streamConfigStreamInput2 :: PC.Input D.Decimal
    }
makeFields ''StreamConfig

-- | Widget update command
data AppCommand
    = AppCounterCommand GTC.CounterCommand
    | QuitCommand

-- | Rendering instruction
-- NB. Free monad is not required for this interpreter
-- * chaining computations are done using Free Monoid of List by the Glazier framework.
-- * no need for monadic control flow in rendering.
data Rendering
    = DisplayText T.Text

type Frontend ctl rndr = (MM.MonoidalMap Char [ctl], [rndr])

counterWindow :: Applicative m => T.Text -> G.Window m GTC.CounterModel (Frontend ctrl Rendering)
counterWindow txt = review G._Window $ \n -> pure (mempty, [DisplayText . T.append txt . T.pack . show $ n])

counterButtonWindow
  :: Applicative m => (GTC.CounterAction -> ctl) -> Char -> T.Text -> GTC.CounterAction -> G.Window m GTC.CounterModel (Frontend ctl Rendering)
counterButtonWindow sendAction c txt action = review G._Window $ \ n -> pure (view (from _Wrapped') $ M.singleton c [control n], [render txt])
  where
    render = DisplayText
    -- NB. Although it's possible to have different control behaviour based on the state
    -- This is not good practice because it could get out of sync if processing becomes busy.
    -- Eg. Say that the control increments in larger amounts as the count becomes larger.
    -- If processing was held up, and there was a backlog of decrements.
    -- all the decrements fired will be large decrements, instead of slowly smaller decrements.
    -- It is much safer to have stateful logic in the `Gadget`, instead of the `Window`.
    control = const $ sendAction action

fieldWindow :: Applicative m => (a -> T.Text) -> G.Window m a (Frontend ctrl Rendering)
fieldWindow f = review G._Window $ \msg -> pure
    ( mempty --ctls
    , let msg' = f msg
      in if T.null $ msg' -- render
      then []
      else pure . DisplayText $ msg'
    )

quitWidget :: Monad m => (GTA.AppAction -> ctl) -> G.Widget GTA.AppModel (Frontend ctl Rendering) m GTA.AppAction [AppCommand]
quitWidget sendAction =
    G.Widget
        (G.Window $
         pure
             ( MM.MonoidalMap $ M.singleton 'q' [sendAction GTA.QuitAction]
             , [DisplayText "Press 'q' to quit"]))
        (G.Gadget $ do
             a <- ask
             lift $
                 case a of
                     GTA.QuitAction -> do
                         GTA.messageModel .= "Quitting"
                         pure [QuitCommand]
                     _ -> pure [])

messageWidget ::
  (GTA.HasMessageModel s T.Text, GTA.AsAppAction a, Monad m) =>
  G.Widget s (MM.MonoidalMap Char [ctrl], [Rendering]) m a [r]
messageWidget =  G.implant GTA.messageModel $ G.dispatch (GTA._AppMessageAction . GTF._FieldAction) $ G.Widget
    (fieldWindow (\s -> (T.append "Message: " s)))
    GTF.fieldGadget

spaceWindow ::
  (Monoid t, Applicative m) => G.Window m s (t, [Rendering])
spaceWindow = G.Window $ pure (mempty, [DisplayText " "])

newlineWindow ::
  (Monoid t, Applicative m) => G.Window m s (t, [Rendering])
newlineWindow = G.Window $ pure (mempty, [DisplayText "\n"])

counterWidget ::
  (GTA.HasCounterModel s Int, GTC.AsCounterAction a, Monad m) =>
  (GTC.CounterAction -> ctl)
  -> G.Widget
       s (MM.MonoidalMap Char [ctl], [Rendering]) m a [GTC.CounterCommand]
counterWidget sendAction' = G.implant GTA.counterModel $ G.dispatch GTC._CounterAction $
   G.Widget
    -- NB. Don't have a counterButtonGadget per buttonWindow - that will mean
    -- an inc/dec action will be evaluated twice!
    -- Ie. consider making update idempotent to avoid manually worrrying about this problem.
    -- Alternatively, have an incrementGadget and decrementGadget.
    (foldMap id $ intersperse spaceWindow
        [ counterButtonWindow sendAction' '+' "Press '+' to increment." GTC.Increment
        , counterButtonWindow sendAction' '-' "Press '-' to decrement." GTC.Decrement
        ])
    (GTC.counterButtonGadget 5000)

counterWidget' ::
  (GTA.HasCounterModel s Int, GTC.AsCounterAction a, Monad m) =>
  (GTC.CounterAction -> ctl)
  -> G.Widget
       s (MM.MonoidalMap Char [ctl], [Rendering]) m a [AppCommand]
counterWidget' sendAction' = fmap AppCounterCommand <$> (counterWidget sendAction')

menuWidget ::
  Monad m =>
  (GTA.AppAction -> ctl)
  -> G.Widget
       GTA.AppModel
       (MM.MonoidalMap Char [ctl], [Rendering])
       m
       GTA.AppAction
       [AppCommand]
menuWidget sendAction = foldMap id $ intersperse (G.statically spaceWindow) [counterWidget' sendAction', quitWidget sendAction]
  where sendAction' = sendAction . GTA.AppCounterAction

counterDisplayWidget ::
  (GTA.HasCounterModel s Int, Monoid c, Monad m) =>
  G.Widget s (MM.MonoidalMap Char [ctrl], [Rendering]) m a c
counterDisplayWidget = G.implant GTA.counterModel $ G.statically $ counterWindow "Current count is: "

signal1Window ::
  (GTS.HasSignal1 s (f (D.DecimalRaw i)), Applicative m,
   Show (f (D.DecimalRaw i)), Functor f, Integral i) =>
  G.Window m s (Frontend ctrl Rendering)
signal1Window = fieldWindow $ \s -> "Signal1: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal1)

signal2Window ::
  (GTS.HasSignal2 s (f (D.DecimalRaw i)), Applicative m,
   Show (f (D.DecimalRaw i)), Functor f, Integral i) =>
  G.Window m s (Frontend ctrl Rendering)
signal2Window = fieldWindow $ \s -> "Signal2: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal2)

ratioWindow ::
  (GTS.HasRatioOfSignals s [D.Decimal], Applicative m) =>
  G.Window m s (Frontend ctrl Rendering)
ratioWindow = fieldWindow $ \s -> "Ratio: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^? (GTS.ratioOfSignals . ix 0))

ratioThresholdCrossedWindow ::
  (GTS.HasRatioThresholdCrossed s a, Applicative m, Show a) =>
  G.Window m s (Frontend ctrl Rendering)
ratioThresholdCrossedWindow = fieldWindow $ \s -> "Crossed?: " `T.append` (T.pack . show $ s ^. GTS.ratioThresholdCrossed)

signalsWindow ::
  (GTS.HasRatioOfSignals s [D.Decimal],
   GTS.HasRatioThresholdCrossed s a,
   GTS.HasSignal1 s (f (D.DecimalRaw i)),
   GTS.HasSignal2 s (f1 (D.DecimalRaw i1)), Applicative m,
   Show (f1 (D.DecimalRaw i1)), Show (f (D.DecimalRaw i)), Show a,
   Functor f1, Functor f, Integral i1, Integral i) =>
  G.Window m s (MM.MonoidalMap Char [ctrl], [Rendering])
signalsWindow = foldMap id $ intersperse newlineWindow[signal1Window, signal2Window, ratioWindow, ratioThresholdCrossedWindow]

signalsWidget ::
  (GTA.HasStreamModel s GTS.StreamModel, GTA.AsAppAction a,
   Monad m) =>
  G.Widget s (MM.MonoidalMap Char [ctrl], [Rendering]) m a [r]
signalsWidget = G.implant GTA.streamModel $ G.dispatch (GTA._SetStreamModel . GTF._FieldAction) $ G.Widget signalsWindow GTF.fieldGadget

appWidget :: Monad m => (GTA.AppAction -> ctl) -> G.Widget GTA.AppModel (Frontend ctl Rendering) m GTA.AppAction [AppCommand]
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
    => AppCommand -> MaybeT io ()
interpretCommand = process
  where
    process QuitCommand = empty

renderFrame' ::
  MonadIO m =>
  (GTA.AppAction -> ctl)
  -> TMVar (MM.MonoidalMap Char [ctl]) -> GTA.AppModel -> m ()
renderFrame' sendAction ctls s = do
    (ctls', frame') <- view (G.window . G._Window) (appWidget sendAction) s
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
ratioSignal c = PFR.reactively $ (/) <$> PFR.React (mkSignal GTS.signal1 (c ^. streamInput1)) <*> PFR.React (mkSignal GTS.signal2 (c ^. streamInput2))

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
   GTS.HasSignal1 s (Maybe D.Decimal),
   GTS.HasSignal2 s (Maybe D.Decimal), MonadState s (t STM),
   MonadTrans t, Alternative (t STM)) =>
  cfg -> P.Producer (Maybe GTS.CrossedDirection) (t STM) ()
thresholdCrossedSignal' c = ratiosSignal' c P.>-> thresholdCrossedPipe
  P.>-> PM.store id GTS.ratioThresholdCrossed

-- | Reactively combine the streamModel signal and the gui Signal
-- Then interpret the resulting command to create an io effect
appSignal :: (Alternative (t STM), MonadState s (t STM)) => P.Producer b (t STM) ()
    -> P.Producer [c] (t STM) ()
    -> P.Producer [c] (t STM) ()
appSignal bs cs = PFR.reactively $ (\bc -> go bc) <$> (PFR.React bs `PFR.merge` PFR.React cs)
  where
    -- In this app, we don't care about the output of the streamModelSignal, just the state effects
    go (Left (_, c)) = c
    go (Right (Left _)) = []
    go (Right (Right (_, c))) = c

-- | This is similar to part of the Elm startApp.
-- This is responsible for setting up the external signal network before running
-- the glazier widget framework using 'runUi'.
exampleApp :: MonadIO io =>
     Int
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

    -- ticker
    let sendAction = (MaybeT . fmap guard) <$> PC.send outputUi
    -- (outputStopwatch, inputStopwatch, sealStopwatch) <- liftIO $ PC.spawn' PC.bounded 1
    -- void . liftIO . forkIO . void . forever . runMaybeT $
    --     (PC.send outputStopwatch)

    -- controls thread
    -- continuously process user input using ctls until it fails (quit)
    -- pushs actions into update thread
    ctls <- liftIO newEmptyTMVarIO
    void . liftIO . forkIO . void . withNoBuffering . runMaybeT . forever $
        interpretControls sendAction ctls

    -- combine the stream signal with the gadget signal
    -- appSignalIO :: P.Producer [AppCommand] (StateT GTA.AppModel STM) ()
    let appSignal' = appSignal
            (thresholdCrossedSignal' (StreamConfig input1 input2))
            (GP.gadgetToProducer inputUi (appWidget sendAction ^. G.gadget))
        -- combine the app signal with interpreting the command output
        -- appSignalIO :: P.Producer [AppCommand] (MaybeT (StateT GTA.AppModel io)) ()
        appSignalIO = hoist (lift . hoist (liftIO . atomically)) appSignal'
        -- interpretCommandsConsumer :: P.Consumer [AppCommand] (MaybeT (StateT GTA.AppModel io)) ()
        interpretCommandsConsumer = hoist (hoist lift) (PP.mapM_ (traverse_ interpretCommand))
        appEffect = appSignalIO P.>-> interpretCommandsConsumer

    -- finally run the main gui threads
    s <- GP.runUi refreshDelay (renderFrame' sendAction ctls) appModel appEffect
    liftIO $ print s
