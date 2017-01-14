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
import qualified Pipes.Misc as PM
-- import qualified Pipes.Prelude as PP
import qualified System.Console.ANSI as ANSI
import qualified System.IO as IO
-- import qualified System.Mem as SM

data StreamConfig = StreamConfig
    { streamConfigStreamInput1 :: PC.Input D.Decimal
    , streamConfigStreamInput2 :: PC.Input D.Decimal
    }
makeFields ''StreamConfig

-- | Widget update command
data AppCommand
    = AppCounterCommand GTC.CounterCommand
    | QuitCommand
    deriving (Eq, Show)

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
quitWidget sendAction = G.Widget
    (G.Window $ pure
        ( MM.MonoidalMap $ M.singleton 'q' [sendAction GTA.Quit]
        , [DisplayText "Press 'q' to quit"]
        )
    )
    (G.Gadget $ do
        a <- ask
        lift $ case a of
            GTA.Quit -> do
                -- FIXME: Could this be a send action?
                GTA.messageModel . GTA.messageIsTemporary .= False -- FIXME: is there a way to make this work as temporary message?
                GTA.messageModel . GTA.messageText .= "Quitting"
                pure [QuitCommand]
            _ -> pure []
    )

messageWidget
    :: (GTA.HasMessageModel s GTA.MessageModel, GTA.AsAppAction b, Monad m)
    => G.Widget s (MM.MonoidalMap Char [ctrl], [Rendering]) m b [r]
messageWidget =  G.implant GTA.messageModel $ G.dispatch (GTA._AppMessageAction . GTF._FieldAction) $ G.Widget
    (fieldWindow (\s -> (T.append "Message: " (s ^. GTA.messageText))))
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
    case M.lookup c (view _Wrapped' ctls') of
      Nothing -> sendAction (GTA.AppMessageAction (GTF.ModifyField $
                                                   \s -> s & GTA.messageIsTemporary .~ True
                                                           & GTA.messageText .~ "Invalid user input"))
      Just xs -> do
          -- reset message on user input
          sendAction (GTA.AppMessageAction (GTF.ModifyField $
                                                   \s -> if s ^. GTA.messageIsTemporary
                                                         then s & GTA.messageIsTemporary .~ False
                                                                & GTA.messageText .~ ""
                                                         else s))
          sequenceA_ xs

withNoBuffering :: IO a -> IO a
withNoBuffering action =
  bracket (IO.hGetBuffering IO.stdin) (IO.hSetBuffering IO.stdin) $ \_ -> do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    action

-- external effect processing - gather commands, and on Cmd, do something
interpretCommands :: (MonadIO io, Traversable t) =>
  (GTA.AppAction -> MaybeT STM ())
  -> t AppCommand
  -> MaybeT io ()
interpretCommands sendAction = traverse_ process
 where
  process QuitCommand = empty
   -- default just show the command
  process a = hoist (liftIO . atomically) $ sendAction (GTA.AppMessageAction . GTF.ModifyField $ \s ->
                                                               s & GTA.messageText .~ T.pack (show a)
                                                                 & GTA.messageIsTemporary .~ True)


-- external effect processing - gather commands, and on Cmd, do something
interpretCommand :: (MonadIO io) =>
  (GTA.AppAction -> MaybeT STM ())
  -> AppCommand
  -> MaybeT io ()
interpretCommand sendAction = process
 where
  process QuitCommand = empty
   -- default just show the command
  process a = hoist (liftIO . atomically) $ sendAction (GTA.AppMessageAction . GTF.ModifyField $ \s ->
                                                               s & GTA.messageText .~ T.pack (show a)
                                                                 & GTA.messageIsTemporary .~ True)

renderFrame' ::
  MonadIO m =>
  (GTA.AppAction -> ctl)
  -> TMVar (MM.MonoidalMap Char [ctl]) -> GTA.AppModel -> m ()
renderFrame' sendAction ctls s = do
    (ctls', frame') <- view (G.window . G._Window) (appWidget sendAction) s
    liftIO . atomically . void $ STE.forceSwapTMVar ctls ctls'
    liftIO $ renderFrame frame'

counterToThresholdCommand :: GTA.HasCounterModel s GTC.CounterModel => s -> GTS.ThresholdCommand
counterToThresholdCommand s = GTS.ThresholdSet . D.Decimal 0 . fromIntegral $ s ^. GTA.counterModel

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
    let sendAction = (MaybeT . fmap guard) <$> PC.send outputUi
    -- (outputStopwatch, inputStopwatch, sealStopwatch) <- liftIO $ PC.spawn' PC.unbounded

    -- controls thread
    -- continuously process user input using ctls until it fails (quit)
    -- pushs actions into update thread
    ctls <- liftIO newEmptyTMVarIO
    void . liftIO . forkIO . void . withNoBuffering . runMaybeT . forever $
        interpretControls sendAction ctls

    -- combine the stream signal with the gadget signal
    let appSignal' = appSignal (thresholdCrossedSignal' (StreamConfig input1 input2)) (gadgetSignal sendAction inputUi)

    -- finally run the main gui threads
    s <- runUi refreshDelay appModel (interpretCommands sendAction) (renderFrame' sendAction ctls) appSignal'
    liftIO $ print s

-- | Stateful signal of commands after consuming an action Producer
-- TODO; move to common package
gadgetSignal ::
  (MonadState GTA.AppModel (t STM), MonadTrans t) =>
  (GTA.AppAction -> ctl)
  -> PC.Input GTA.AppAction
  -> P.Producer [AppCommand] (t STM) ()
gadgetSignal sendAction input = hoist lift (PM.fromInputSTM input) P.>-> GP.gadgetToPipe (appWidget sendAction ^. G.gadget)

-- | TODO: move to Pipes.Misc
onState :: (MonadState s m) => (s -> m ()) -> P.Pipe a a m r
onState f = P.for P.cat $ \a -> do
    s <- get
    lift $ f s
    P.yield a

-- | This is similar to part of the Elm startApp.
-- This is responsible for running the glazier widget update tick until it quits.
-- This is also responsible for rendering the frame and interpreting commands.
-- TODO: move to Glazier.Pipes
runUi :: (MonadIO io) =>
     Int
  -> s
  -> (cmd -> MaybeT IO ()) -- interpretCmds
  -> (s -> MaybeT IO ()) -- render
  -> P.Producer cmd (StateT s STM) ()
  -> io s
runUi refreshDelay initialState interpretCmds render appSig
    -- framerate thread
    -- TMVar to indicate that the render thread can render, start non empty so we can render straight away.
 = do
    triggerRender <- liftIO $ newTMVarIO ()
    frameRateThread <-
        liftIO $
        forkIO . void . forever $
        -- The will ensure a refreshDelay in between times when value in canRender is taken.
        -- wait until canRender is empty (ie taken by the render thread)
         do
            atomically $ STE.waitTillEmptyTMVar triggerRender ()
            -- if empty, then wait delay before filling TMVar with next canRender value
            threadDelay refreshDelay
            atomically $ putTMVar triggerRender ()

    -- render thread
    enableRenderThread <- liftIO $ newTMVarIO ()
    finishedRenderThread <- liftIO newEmptyTMVarIO
    latestState <- liftIO $ newTMVarIO initialState
    void . liftIO $
        forkFinally
            (void . runMaybeT . forever $
              -- check if we can start render
              do
                 liftIO . atomically . void $ takeTMVar triggerRender
                 -- to allow rendering of last frame before quitting
                 -- if there is no state to render, check if rendering is disabled
                 s <-
                     MaybeT . liftIO . atomically $
                     (Just <$> takeTMVar latestState) `orElse` do
                         r <- tryReadTMVar enableRenderThread
                         case r of
                             Nothing -> pure Nothing -- breaks (runMaybeT . forever) loop
                             Just _ -> retry
                 render s)
            (const . atomically $ putTMVar finishedRenderThread ())
    -- hoist to atomically apply the STM
    -- then lift from StateT s IO -> MaybeT (StateT s IO)
    s' <-
        liftIO $
        P.runEffect $
        PL.execStateP initialState $
        PL.runMaybeP $
        -- hoist MaybeT STM -> MaybeT (StateT STM), then lift into the Pipe
        P.for (hoist
                  (lift . hoist (liftIO . atomically))
                  (appSig P.>-> onState (void . lift . STE.forceSwapTMVar latestState))) $ \c ->
            lift $ hoist lift (interpretCmds c)

    -- cleanup
    -- allow rendering of the frame one last time
    liftIO . atomically $ takeTMVar enableRenderThread
    -- wait for render thread to finish before exiting
    liftIO . atomically $ takeTMVar finishedRenderThread
    -- kill frameRateThread only after render thread has finished
    -- since renderThread waits on triggers from frameRateThread
    liftIO $ killThread frameRateThread
    -- liftIO $ killThread ctlsThread
    -- return final state
    liftIO $ pure s'
