{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.Tutorial.Console where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
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
import qualified Control.Monad.Trans.State.Strict.Extras as SE
import Data.Bifunctor
import qualified Data.Decimal as D
import Data.Foldable
import Data.List (intersperse)
import qualified Data.Map.Monoidal.Strict as MM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Glazier as G
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
import qualified System.Console.ANSI as ANSI
import qualified System.IO as IO
import qualified System.Mem as SM

-- | Widget update command
data AppCommand = AppCounterCommand GTC.CounterCommand | QuitCommand | AppThresholdCommand GTS.ThresholdCommand
  deriving (Eq, Show)

-- | Rendering instruction
-- NB. Free monad is not required for this interpreter
-- * chaining computations are done using Free Monoid of List by the Glazier framework.
-- * no need for monadic control flow in rendering.
data Rendering
  = DisplayText T.Text

type Frontend ctl rndr = (MM.MonoidalMap Char [ctl], [rndr])

depictCounter :: T.Text ->  G.Depict GTC.CounterModel (Frontend ctrl Rendering)
depictCounter txt = G.Depict $ \n -> (mempty, [DisplayText . T.append txt . T.pack . show $ n])

depictCounterButton
  :: (GTC.CounterAction -> ctl) -> Char -> T.Text -> GTC.CounterAction -> G.Depict GTC.CounterModel (Frontend ctl Rendering)
depictCounterButton sendAction c txt action = G.Depict $ \ n -> (view (from _Wrapped') $ M.singleton c [control n], [render txt])
 where
  render = DisplayText
  -- NB. Although it's possible to have different control behaviour based on the state
  -- This is not good practice because it could get out of sync if processing becomes busy.
  -- Eg. Say that the control increments in larger amounts as the count becomes larger.
  -- If processing was held up, and there was a backlog of decrements.
  -- all the decrements fired will be large decrements, instead of slowly smaller decrements.
  -- It is much safer to have stateful logic in the `Notify`, instead of the `Depict`.
  control = const $ sendAction action

depictField :: (a -> T.Text) -> G.Depict a (Frontend ctrl Rendering)
depictField f = G.Depict $ \msg ->
  ( mempty --ctls
  , let msg' = f msg
    in if T.null $ msg' -- render
    then []
    else pure . DisplayText $ msg'
  )

quitWidget :: (GTA.AppAction -> ctl) -> G.Widget GTA.AppAction GTA.AppModel [AppCommand] (Frontend ctl Rendering)
quitWidget sendAction = G.Widget
  (G.Notify $ do
    a <- ask
    lift $ case a of
      GTA.Quit -> do
          -- FIXME: Could this be a send action?
          GTA.messageModel . GTA.messageIsTemporary .= False -- FIXME: is there a way to make this work as temporary message?
          GTA.messageModel . GTA.messageText .= "Quitting"
          pure [QuitCommand]
      _ -> pure []
  )
  (G.Depict $ const
    ( MM.MonoidalMap $ M.singleton 'q' [sendAction GTA.Quit] -- MonoidalMap is accidentally hidden. Will be fixed in monoidal-containers >= 0.3.0.1
    , [DisplayText "Press 'q' to quit"]
    )
  )

messageWidget ::
  (GTA.HasMessageModel s GTA.MessageModel, GTA.AsAppAction b) =>
  G.Widget b s [r] (MM.MonoidalMap Char [ctrl], [Rendering])
messageWidget =  G.implant GTA.messageModel $ G.dispatch (GTA._AppMessageAction . GTF._FieldAction) $ G.Widget
 GTF.notifyField
 (depictField (\s -> T.append "Message: " (s ^. GTA.messageText)))

appWidget :: (GTA.AppAction -> ctl) -> G.Widget GTA.AppAction GTA.AppModel [AppCommand] (Frontend ctl Rendering)
appWidget sendAction = foldMap id $
  intersperse (G.statically depictNewline)
  [ G.statically depictNewline
  , messageWidget
  , counterDisplayWidget
  , signalsWidget
  , menuWidget
  , G.statically depictNewline
  ]
 where

  sendAction' = sendAction . GTA.AppCounterAction

  counterDisplayWidget = G.implant GTA.counterModel $ G.statically $ depictCounter "Current count is: "

  depictSpace = G.Depict $ const (mempty, [DisplayText " "])

  depictNewline = G.Depict $ const (mempty, [DisplayText "\n"])

  counterWidget = G.implant GTA.counterModel $ G.dispatch GTC._CounterAction $ G.Widget
    -- NB. Don't have a counterButtonNotify per buttonDepict - that will mean
    -- an inc/dec action will be evaluated twice!
    -- Ie. consider making update idempotent to avoid manually worrrying about this problem.
    -- Alternatively, have an incrementNotify and decrementNotify.
    (GTC.notifyCounterButton 5000)
    (foldMap id $ intersperse depictSpace
      [ depictCounterButton sendAction' '+' "Press '+' to increment." GTC.Increment
      , depictCounterButton sendAction' '-' "Press '-' to decrement." GTC.Decrement
      ])
  counterWidget' = fmap AppCounterCommand `first` counterWidget
  notifyThreshold' = fmap AppThresholdCommand <$> GTS.notifyThreshold (Just . counterToThresholdCommand)
  counterWidget'' = counterWidget' `mappend` G.dynamically notifyThreshold'

  menuWidget = foldMap id $ intersperse (G.statically depictSpace) [counterWidget'', quitWidget sendAction]

  depictSignal1 = depictField $ \s -> "Signal1: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal1)

  depictSignal2 = depictField $ \s -> "Signal2: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal2)

  depictRatio = depictField $ \s -> "Ratio: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^? (GTS.ratioOfSignals . ix 0))

  depictRatioThresholdCrossed = depictField $ \s -> "Crossed?: " `T.append` (T.pack . show $ s ^. GTS.ratioThresholdCrossed)

  depictSignals = foldMap id $ intersperse depictNewline[depictSignal1, depictSignal2, depictRatio, depictRatioThresholdCrossed]

  signalsWidget = G.implant GTA.streamModel $ G.dispatch (GTA._SetStreamModel . GTF._FieldAction) $ G.Widget GTF.notifyField depictSignals

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
  -> TVar D.Decimal
  -> t AppCommand
  -> MaybeT io ()
interpretCommands sendAction threshold = traverse_ process
 where
  process QuitCommand = empty
  process (AppThresholdCommand (GTS.ThresholdSet t)) = liftIO . atomically $ writeTVar threshold t
   -- default just show the command
  process a = hoist (liftIO . atomically) $ sendAction (GTA.AppMessageAction . GTF.ModifyField $ \s ->
                                                               s & GTA.messageText .~ T.pack (show a)
                                                                 & GTA.messageIsTemporary .~ True)

-- | This is similar to part of the Elm startApp.
-- This is responsible for running the glazier widget update tick until it quits.
-- This is also responsible for rendering the frame and interpreting commands.
runUi :: MonadIO io =>
     Int
  -> GTA.AppModel
  -> PC.Output GTA.AppAction
  -> PC.Input GTA.AppAction
  -> TVar D.Decimal -- TODO: Make more generic
  -> io GTA.AppModel
runUi refreshDelay initialState address inbox threshold
 = do
    -- sendAction :: GTA.AppAction -> MaybeT STM ()
    let sendAction = (MaybeT . fmap guard) <$> PC.send address
        (xs, render) = G.startWidget (appWidget sendAction) inbox
        tickState =
            SE.maybeState $ hoist (MaybeT . liftIO . atomically . PC.recv) xs
    -- controls thread
    -- continuously process user input using ctls until it fails (quit)
    -- pushs actions into update thread
    ctls <- liftIO newEmptyTMVarIO
    ctlsThread <-
        liftIO $
        forkIO . void . withNoBuffering . runMaybeT . forever $
        interpretControls sendAction ctls
    -- framerate thread
    -- TMVar to indicate that the render thread can render, start non empty so we can render straight away.
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
    frameState <- liftIO newEmptyTMVarIO
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
                     (Just <$> takeTMVar frameState) `orElse` do
                         r <- tryReadTMVar enableRenderThread
                         case r of
                             Nothing -> pure Nothing -- breaks (runMaybeT . forever) loop
                             Just _ -> retry
                 let (ctls', frame') = render s
                 liftIO . atomically . void $ STE.forceSwapTMVar ctls ctls'
                 liftIO $ renderFrame frame')
            (const . atomically $ putTMVar finishedRenderThread ())
    -- this is called after each tick with the new state to render
    let onFrame s = void . liftIO . atomically $ STE.forceSwapTMVar frameState s
    s' <-
        liftIO . (`execStateT` initialState) . runMaybeT $
        -- With 'G.startWidget', tick will be blocked if there is nothing in
        -- the address inbox, ie if 'send' was not used.
        -- So send a dummy action to start things off, otherwise we'll get
        -- STM blocked indefinitely exception
         do
            hoist (liftIO . atomically) (sendAction GTA.Redraw)
            -- update thread. Loop while there is a frame and no error or quit
            forever $
                G.runNotify
                    tickState
                    onFrame
                    (interpretCommands sendAction threshold)
    -- cleanup
    -- allow rendering of the frame one last time
    liftIO . atomically $ takeTMVar enableRenderThread
    -- wait for render thread to finish before exiting
    liftIO . atomically $ takeTMVar finishedRenderThread
    -- kill frameRateThread only after render thread has finished
    -- since renderThread waits on triggers from frameRateThread
    liftIO $ killThread frameRateThread
    liftIO $ killThread ctlsThread
    -- return final state
    liftIO $ pure s'

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
  -- (outputStopwatch, inputStopwatch, sealStopwatch) <- liftIO $ PC.spawn' PC.unbounded

  -- initialize the threshold TVar to share between the signal network and widget
  let GTS.ThresholdSet initialThreshold = counterToThresholdCommand appModel
  threshold <- liftIO $ newTVarIO initialThreshold

  -- fork a thread that continously outputs prodSigModel to outputUi
  void . liftIO . forkIO . void . P.runEffect $ hoist atomically (streamModelSignal input1 input2 threshold initialStreamModel) P.>-> consumeStreamModel outputUi

  -- finally run the main gui threads
  void $ runUi refreshDelay appModel outputUi inputUi threshold
 where
     initialStreamModel = appModel ^. GTA.streamModel

counterToThresholdCommand :: GTA.HasCounterModel s GTC.CounterModel => s -> GTS.ThresholdCommand
counterToThresholdCommand s = GTS.ThresholdSet . D.Decimal 0 . fromIntegral $ s ^. GTA.counterModel

-- signal network
mkSignal :: Lens' s (Maybe D.Decimal) -> PC.Input D.Decimal -> P.Producer D.Decimal (StateT s STM) ()
mkSignal lns input = P.hoist lift (PM.fromInputSTM input) P.>-> PM.store (to Just) lns

ratioSignal :: PC.Input D.Decimal -> PC.Input D.Decimal -> P.Producer D.Decimal (StateT GTS.StreamModel STM) ()
ratioSignal input1 input2 = PFR._reactively $ (/) <$> PFR.React (mkSignal GTS.signal1 input1) <*> PFR.React (mkSignal GTS.signal2 input2)

ratiosSignal' :: PC.Input D.Decimal -> PC.Input D.Decimal -> P.Producer [D.Decimal] (StateT GTS.StreamModel STM) ()
ratiosSignal' input1 input2 = ratioSignal input1 input2 P.>-> PM.buffer 2 [] P.>-> PM.store id GTS.ratioOfSignals

thresholdCrossedSignal :: TVar D.Decimal -> P.Pipe [D.Decimal] (Maybe GTS.CrossedDirection) STM ()
thresholdCrossedSignal threshold = P.for P.cat $ \rs -> do
  t <- lift $ readTVar threshold
  let r = rs ^? ix 0
      prevR = rs ^? ix 1
  P.yield (GTS.thresholdCrossed (Just t) r prevR)

-- TODO: rule for threshold crossed
-- Just (Just x) -> use Just x
-- Nothing -> use previous Just value
-- Timer expired (Just Nothing) -> use nothing
-- This is only possible with animation
-- Which means we need a signal of: t = (TimePassed, AbsTime)
-- Then we can create signal: y_next = let y' = y_prev - TimePassed in if y' > 0 then y' else 0
thresholdCrossedSignal' :: PC.Input D.Decimal -> PC.Input D.Decimal -> TVar D.Decimal -> P.Producer (Maybe GTS.CrossedDirection) (StateT GTS.StreamModel STM) ()
thresholdCrossedSignal' input1 input2 threshold = ratiosSignal' input1 input2 P.>-> hoist lift (thresholdCrossedSignal threshold)
  P.>-> PM.store id GTS.ratioThresholdCrossed

-- output signal
streamModelSignal :: PC.Input D.Decimal -> PC.Input D.Decimal -> TVar D.Decimal -> GTS.StreamModel -> P.Producer GTS.StreamModel STM ()
streamModelSignal input1 input2 threshold initialInputModel = PL.evalStateP initialInputModel (thresholdCrossedSignal' input1 input2 threshold P.>-> PM.retrieve id)

consumeStreamModel :: PC.Output GTA.AppAction -> P.Consumer GTS.StreamModel IO ()
consumeStreamModel outputUi = do
  -- await atomically, as it's impossible to await all values in one transaction
  a <- P.await
  b <- lift $ atomically $ PC.send outputUi (GTA.SetStreamModel . GTF.SetField $ a)
  if b
    then consumeStreamModel outputUi
    else pure ()
