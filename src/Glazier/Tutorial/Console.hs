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
import qualified Control.Concurrent as C
import Control.Concurrent.STM
import qualified Control.Concurrent.Extra as CE
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
depictCounterButton mkCtl c txt action = G.Depict $ \ n -> (view (from _Wrapped') $ M.singleton c [control n], [render txt])
 where
  render = DisplayText
  -- NB. Although it's possible to have different control behaviour based on the state
  -- This is not good practice because it could get out of sync if processing becomes busy.
  -- Eg. Say that the control increments in larger amounts as the count becomes larger.
  -- If processing was held up, and there was a backlog of decrements.
  -- all the decrements fired will be large decrements, instead of slowly smaller decrements.
  -- It is much safer to have stateful logic in the `Notify`, instead of the `Depict`.
  control = const $ mkCtl action

depictField :: (a -> T.Text) -> G.Depict a (Frontend ctrl Rendering)
depictField f = G.Depict $ \msg ->
  ( mempty --ctls
  , let msg' = f msg
    in if T.null msg' -- render
    then []
    else pure . DisplayText $ msg'
  )

quitWidget :: (GTA.AppAction -> ctl) -> G.Widget GTA.AppAction n [AppCommand] (Frontend ctl Rendering)
quitWidget mkCtl = G.Widget
  (G.Notify $ do
    a <- ask
    lift $ case a of
      GTA.Quit -> pure [QuitCommand]
      _ -> pure []
  )
  (G.Depict $ const
    -- ( MM.MonoidalMap $ M.singleton 'q' [mkCtl GTA.Close] -- MonoidalMap is accidentally hidden. Will be fixed in monoidal-containers >= 0.3.0.1
    ( view (from _Wrapped') $ M.singleton 'q' [mkCtl GTA.Quit]
    , [DisplayText "Press 'q' to quit"]
    )
  )

appWidget :: (GTA.AppAction -> ctl) -> G.Widget GTA.AppAction GTA.AppModel [AppCommand] (Frontend ctl Rendering)
appWidget mkCtl = foldMap id $
  intersperse (G.statically depictNewline)
  [ G.statically depictNewline
  , messageWidget
  , counterDisplayWidget
  , signalsWidget
  , menuWidget
  , G.statically depictNewline
  ]
 where
  mkCtl' = mkCtl . GTA.AppCounterAction

  messageWidget =  G.implant GTA.messageModel $ G.dispatch (GTA._AppMessageAction . GTF._FieldAction) $ G.Widget
     GTF.notifyField
     (depictField $ T.append "Message: ")

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
      [ depictCounterButton mkCtl' '+' "Press '+' to increment." GTC.Increment
      , depictCounterButton mkCtl' '-' "Press '-' to decrement." GTC.Decrement
      ])
  counterWidget' = fmap AppCounterCommand `first` counterWidget
  notifyThreshold' = fmap AppThresholdCommand <$> GTS.notifyThreshold (Just . counterToThresholdCommand)
  counterWidget'' = counterWidget' `mappend` G.dynamically notifyThreshold'

  menuWidget = foldMap id $ intersperse (G.statically depictSpace) [counterWidget'', quitWidget mkCtl]

  depictSignal1 = depictField $ \s -> "Signal1: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal1)

  depictSignal2 = depictField $ \s -> "Signal2: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal2)

  depictRatio = depictField $ \s -> "Ratio: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^? (GTS.ratioOfSignals . ix 0))

  depictRatioThresholdCrossed = depictField $ \s -> "Crossed?: " `T.append` (T.pack . show $ s ^. GTS.ratioThresholdCrossed)

  depictSignals = foldMap id $ intersperse depictNewline[depictSignal1, depictSignal2, depictRatio, depictRatioThresholdCrossed]

  signalsWidget = G.implant GTA.streamModel $ G.dispatch (GTA._SetStreamModel . GTF._FieldAction) $ G.Widget GTF.notifyField depictSignals

-- | This is similar to part of the Elm startApp.
-- This is responsible for running the glazier widget update tick until it quits.
-- This is also responsible for rendering the frame and interpreting commands.
startUi :: MonadIO io =>
     Int
  -> GTA.AppModel
  -> PC.Output GTA.AppAction
  -> PC.Input GTA.AppAction
  -> STM ()
  -> TVar D.Decimal -- TODO: Make more generic
  -> io GTA.AppModel
startUi refreshDelay initialState address inbox seal threshold = do
  -- sendAction :: GTA.AppAction -> MaybeT STM ()
  let sendAction = (MaybeT . fmap guard) <$> PC.send address
      (xs, render) = G.startWidget (appWidget sendAction) inbox
      tickState = SE.maybeState $ hoist (MaybeT . liftIO . atomically . PC.recv) xs
  -- controls thread
  -- continuously process user input using ctls until it fails (quit)
  -- pushs actions into update thread
  ctls <- liftIO newEmptyTMVarIO
  ctlsThread <- liftIO $ C.forkIO . void . withNoBuffering . runMaybeT . forever $ interpretControls sendAction ctls

  -- framerate thread
  -- TMVar to indicate that the render thread can render, start non empty so we can render straight away.
  canRender <- liftIO $ newTMVarIO ()
  frameRateThread <- liftIO $ C.forkIO . void . forever $ do
      -- wait until canRender is empty
      atomically $ do
          putTMVar canRender ()
          void $ takeTMVar canRender
      -- if empty, then wait delay before filling TMVar with next canRender message
      C.threadDelay refreshDelay
      atomically $ putTMVar canRender ()

  -- render thread
  finishedRenderThread <- liftIO newEmptyTMVarIO
  (renderOutput, renderInput, renderSeal) <- liftIO . PC.spawn' $ PC.newest 1
  void $ liftIO $ CE.forkFinally
      (void . runMaybeT . forever $ renderFrame renderInput)
      (const . atomically $ putTMVar finishedRenderThread ())

  -- this is called after each tick with the new state to render
  let onFrame s =  MaybeT . liftIO . atomically $ tryRender s <|> pure (Just ())
      tryRender s = do
          -- check if we can render
          void $ takeTMVar canRender
          let (ctls', frame') = render s
          -- store latest controls and rendering frame
          void $ forceSwapTMVar ctls ctls'
          guard <$> PC.send renderOutput frame'

      quit = seal >> renderSeal

  s' <- liftIO . (`execStateT` initialState) . runMaybeT $ do
        -- With 'G.startWidget', tick will be blocked if there is nothing in
        -- the address inbox, ie if 'send' was not used.
        -- So send a dummy action to start things off, otherwise we'll get
        -- STM blocked indefinitely exception
        hoist (liftIO . atomically) (sendAction GTA.Redraw)
        -- update thread. Loop while there is a frame and no error or quit
        forever $ G.runNotify tickState onFrame (interpretCommands sendAction quit threshold)

  -- cleanup
  liftIO $ atomically quit -- just in case we go here due to a render error
  liftIO $ C.killThread ctlsThread
  liftIO $ C.killThread frameRateThread
  -- wait for render thread to finish before exiting
  liftIO . atomically $ takeTMVar finishedRenderThread
  -- return final state
  liftIO $ pure s'

renderFrame :: MonadIO io => PC.Input [Rendering] -> MaybeT io ()
renderFrame frame = do
  frame' <- MaybeT $ liftIO $ atomically $ PC.recv frame
  liftIO ANSI.clearScreen
  liftIO $ traverse_ process frame'
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
    sendAction (GTA.AppMessageAction . GTF.SetField $ mempty) -- reset message on user input
    case M.lookup c (view _Wrapped' ctls') of
      Nothing -> sendAction (GTA.AppMessageAction . GTF.SetField $ "Invalid user input")
      Just xs -> sequenceA_ xs

withNoBuffering :: IO a -> IO a
withNoBuffering action =
  bracket (IO.hGetBuffering IO.stdin) (IO.hSetBuffering IO.stdin) $ \_ -> do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    action

-- external effect processing - gather commands, and on Cmd, do something
interpretCommands :: (MonadIO io,  Traversable t) =>
  (GTA.AppAction -> MaybeT STM ())
  -> STM ()
  -> TVar D.Decimal
  -> t AppCommand
  -> MaybeT io ()
interpretCommands sendAction seal threshold = traverse_ process
 where
  process QuitCommand = do
    liftIO $ atomically seal
    empty
  process (AppThresholdCommand (GTS.ThresholdSet t)) = liftIO . atomically $ writeTVar threshold t
   -- default just show the command
  process a = hoist (liftIO . atomically) $ sendAction (GTA.AppMessageAction . GTF.SetField $ T.pack $ show a)

-- | forces a swap of TMVar irreguardless if it previously contained a value or not.
-- returns what was in the TMVar (if exists)
forceSwapTMVar :: TMVar a -> a -> STM (Maybe a)
forceSwapTMVar v a = (Just <$> swapTMVar v a) `orElse` (const Nothing <$> putTMVar v a)

-- | This is similar to part of the Elm startApp.
-- This is responsible for setting up the external signal network before running
-- the glazier widget framework using 'startUi'.
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
  (output1, input1, seal1) <- liftIO . PC.spawn' $ PC.newest 1
  void . liftIO . C.forkIO . void . (`execStateT` start1) . forever . runMaybeT $ GTR.randomSignal
    tick1
    desired1
    threshold1
    interval1
    (PC.send output1)

  (output2, input2, seal2) <- liftIO . PC.spawn' $ PC.newest 1
  void . liftIO . C.forkIO . void . (`execStateT` start2) . forever . runMaybeT $ GTR.randomSignal
    tick2
    desired2
    threshold2
    interval2
    (PC.send output2)

  -- threads for rendering and controlling UI
  (outputUi, inputUi, sealUi) <- liftIO $ PC.spawn' PC.unbounded
  -- (outputStopwatch, inputStopwatch, sealStopwatch) <- liftIO $ PC.spawn' PC.unbounded

  -- initialize the threshold TVar to share between the signal network and widget
  let GTS.ThresholdSet initialThreshold = counterToThresholdCommand appModel
  threshold <- liftIO $ newTVarIO initialThreshold

  -- fork a thread that continously outputs prodSigModel to outputUi
  void . liftIO . C.forkIO . void . P.runEffect $ hoist atomically (streamModelSignal input1 input2 threshold initialStreamModel) P.>-> consumeStreamModel outputUi

  -- finally run the main gui threads
  void $ startUi refreshDelay appModel outputUi inputUi (seal1 >> seal2 >> sealUi) threshold
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
