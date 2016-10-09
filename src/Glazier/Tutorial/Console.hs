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
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import qualified Data.Decimal as D
import Data.Foldable
import Data.List (intersperse)
import qualified Data.Map.Monoidal.Strict as MM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Glazier as G
import qualified Glazier.Tutorial.App as GTA
import qualified Glazier.Tutorial.Counter as GTC
import qualified Glazier.Tutorial.Field as GTF
import qualified Glazier.Tutorial.IO as GTI
import qualified Glazier.Tutorial.Random as GTR
import qualified Glazier.Tutorial.SignalModel as GTS
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Fluid.React as PFR
import qualified Pipes.Lift as PL
import qualified Pipes.Misc as PM
import qualified System.Console.ANSI as ANSI
import qualified System.IO as IO

-- | Widget update command
data AppCommand = AppCounterCommand GTC.CounterCommand | Quit | AppThresholdCommand GTS.ThresholdCommand
  deriving (Eq, Show)

-- | Rendering instruction
-- NB. Free monad is not required for this interpreter
-- * chaining computations are done using Free Monoid of List by the Glazier framework.
-- * no need for monadic control flow in rendering.
data Render
  = DisplayText T.Text

type Frontend ctl rndr = (MM.MonoidalMap Char [ctl], [rndr])

counterView :: T.Text ->  G.View GTC.CounterModel (Frontend ctrl Render)
counterView txt = G.View $ \n -> (mempty, [DisplayText . T.append txt . T.pack . show $ n])

counterButtonView
  :: (GTC.CounterAction -> ctl) -> Char -> T.Text -> GTC.CounterAction -> G.View GTC.CounterModel (Frontend ctl Render)
counterButtonView mkCtl c txt action = G.View $ \ n -> (view (from _Wrapped') $ M.singleton c [control n], [render txt])
 where
  render = DisplayText
  -- NB. Although it's possible to have different control behaviour based on the state
  -- This is not good practice because it could get out of sync if processing becomes busy.
  -- Eg. Say that the control increments in larger amounts as the count becomes larger.
  -- If processing was held up, and there was a backlog of decrements.
  -- all the decrements fired will be large decrements, instead of slowly smaller decrements.
  -- It is much safer to have stateful logic in the `Update`, instead of the `View`.
  control = const $ mkCtl action

fieldView :: (a -> T.Text) -> G.View a (Frontend ctrl Render)
fieldView f = G.View $ \msg ->
  ( mempty --ctls
  , let msg' = f msg
    in if T.null msg' -- render
    then []
    else pure . DisplayText $ msg'
  )

quitWidget :: (GTA.AppAction -> ctl) -> G.Widget GTA.AppAction n [AppCommand] (Frontend ctl Render)
quitWidget mkCtl = G.Widget
  (G.Update $ do
    a <- ask
    lift $ case a of
      GTA.Close -> pure [Quit]
      _ -> pure []
  )
  (G.View $ const
    -- ( MM.MonoidalMap $ M.singleton 'q' [mkCtl GTA.Close] -- MonoidalMap is accidentally hidden. Will be fixed in monoidal-containers >= 0.3.0.1
    ( view (from _Wrapped') $ M.singleton 'q' [mkCtl GTA.Close]
    , [DisplayText "Press 'q' to quit"]
    )
  )

toThresholdCommand :: GTA.HasAppModel s => s -> GTS.ThresholdCommand
toThresholdCommand s = GTS.ThresholdSet . D.Decimal 0 . fromIntegral $ s ^. GTA.appCounterModel

appWidget :: (GTA.AppAction -> ctl) -> G.Widget GTA.AppAction GTA.AppModel [AppCommand] (Frontend ctl Render)
appWidget mkCtl = foldMap id $
  intersperse (G.statically newlineView)
  [ G.statically newlineView
  , messageWidget
  , counterDisplayWidget
  , signalsWidget
  , menuWidget
  , G.statically newlineView
  ]
 where
  mkCtl' = mkCtl . GTA.AppCounterAction

  messageWidget =  G.implant GTA.appMessageModel $ G.dispatch (GTA._AppMessageAction . GTF._FieldAction) $ G.Widget
     GTF.fieldUpdate
     (fieldView $ T.append "Message: ")

  counterDisplayWidget = G.implant GTA.appCounterModel $ G.statically $ counterView "Current count is: "

  spaceView = G.View $ const (mempty, [DisplayText " "])

  newlineView = G.View $ const (mempty, [DisplayText "\n"])

  counterWidget = G.implant GTA.appCounterModel $ G.dispatch GTC._CounterAction $ G.Widget
    -- NB. Don't have a counterButtonUpdate per buttonView - that will mean
    -- an inc/dec action will be evaluated twice!
    -- Ie. consider making update idempotent to avoid manually worrrying about this problem.
    -- Alternatively, have an incrementUpdate and decrementUpdate.
    (GTC.counterButtonUpdate 5000)
    (foldMap id $ intersperse spaceView
      [ counterButtonView mkCtl' '+' "Press '+' to increment." GTC.Increment
      , counterButtonView mkCtl' '-' "Press '-' to decrement." GTC.Decrement
      ])
  counterWidget' = fmap AppCounterCommand `first` counterWidget
  thresholdUpdate' = fmap AppThresholdCommand <$> GTS.thresholdUpdate (Just . toThresholdCommand)
  counterWidget'' = counterWidget' `mappend` G.dynamically thresholdUpdate'

  menuWidget = foldMap id $ intersperse (G.statically spaceView) [counterWidget'', quitWidget mkCtl]

  signal1View = fieldView $ \s -> "Signal1: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal1)

  signal2View = fieldView $ \s -> "Signal2: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^. GTS.signal2)

  ratioView = fieldView $ \s -> "Ratio: " `T.append` (T.pack . show $ D.roundTo 2 <$> s ^? (GTS.ratioOfSignals . ix 0))

  ratioThresholdCrossedView = fieldView $ \s -> "Crossed?: " `T.append` (T.pack . show $ s ^. GTS.ratioThresholdCrossed)

  signalsView = foldMap id $ intersperse newlineView [signal1View, signal2View, ratioView, ratioThresholdCrossedView]

  signalsWidget = G.implant GTA.appSignalModel $ G.dispatch (GTA._SetSignalModel . GTF._FieldAction) $ G.Widget GTF.fieldUpdate signalsView

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
  let mkCtl = (MaybeT . fmap guard) <$> PC.send address
      xs = G.startWidget (appWidget mkCtl) inbox
      getTick s' = PC.recv (runStateT xs s')
      -- always render at most every period microseconds
      period = refreshDelay
  -- controls thread
  -- continuously process user input using ctls until it fails (quit)
  -- pushs actions into update thread
  ctls <- liftIO newEmptyTMVarIO
  ctlsThread <- liftIO $ C.forkIO . void . withNoBuffering . runMaybeT . forever $ interpretControls mkCtl ctls

  -- render thread
  frame <- liftIO newEmptyTMVarIO
  frameThread <- liftIO $ C.forkIO . void . forever . GTI.intermittently period $ renderFrame frame

  let onFrame (ctls', frame') = liftIO $ atomically $ do
          -- store latest controls and rendering frame
          void $ forceSwapTMVar ctls ctls'
          void $ forceSwapTMVar frame frame'

  s' <- liftIO $ runMaybeT $ do
    -- With 'G.startWidget', there is no tick if there is nothing in
    -- the address inbox, ie if 'send' was not used.
    -- So send a dummy action to start things off, otherwise we'll get
    -- STM blocked indefinitely exception
    hoist atomically (mkCtl GTA.Redraw)
    -- update thread. Loop while there is a frame and no error
    finalState <- runExceptT . (`execStateT` initialState) . forever $
      G.runUpdate onFrame (interpretCommands mkCtl seal threshold) (MaybeT . liftIO . atomically . getTick)
    either pure pure finalState
  liftIO $ C.killThread ctlsThread
  liftIO $ C.killThread frameThread
  liftIO $ pure $ fromMaybe initialState s'

renderFrame :: MonadIO io => TMVar [Render]-> io ()
renderFrame frame = do
  frame' <- liftIO $ atomically $ takeTMVar frame
  liftIO ANSI.clearScreen
  liftIO $ traverse_ process frame'
 where
  process (DisplayText txt) = putStr . T.unpack $ txt

-- | Get user input and pump into address AppAction
-- TODO: get rid of STM Bool, use MaybeT STM ()
interpretControls :: MonadIO io =>
  (GTA.AppAction -> MaybeT STM ())
  -> TMVar (MM.MonoidalMap Char [MaybeT STM ()])
  -> MaybeT io ()
interpretControls mkCtl ctls = do
  c <- liftIO getChar
  hoist (liftIO . atomically) $ do
    ctls' <- lift $ readTMVar ctls
    mkCtl (GTA.AppMessageAction . GTF.SetField $ mempty) -- reset message on user input
    case M.lookup c (view _Wrapped' ctls') of
      Nothing -> mkCtl (GTA.AppMessageAction . GTF.SetField $ "Invalid user input")
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
interpretCommands mkCtl seal threshold = traverse_ process
 where
  process Quit = do
    liftIO $ atomically seal
    empty
  process (AppThresholdCommand (GTS.ThresholdSet t)) = liftIO . atomically $ writeTVar threshold t
   -- default just show the command
  process a = hoist (liftIO . atomically) $ mkCtl (GTA.AppMessageAction . GTF.SetField $ T.pack $ show a)

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

  -- initialize the threshold TVar to share between the signal network and widget
  let GTS.ThresholdSet initialT = toThresholdCommand appModel
  threshold <- liftIO $ newTVarIO initialT

  -- signal network
  let
    prod1 :: P.Producer D.Decimal (StateT GTS.SignalModel STM) ()
    prod1 = P.hoist lift (PM.fromInputSTM input1) P.>-> PM.store (to Just) GTS.signal1

    prod2 :: P.Producer D.Decimal (StateT GTS.SignalModel STM) ()
    prod2 = P.hoist lift (PM.fromInputSTM input2) P.>-> PM.store (to Just) GTS.signal2

    prodRatio :: P.Producer D.Decimal (StateT GTS.SignalModel STM) ()
    prodRatio = PFR.reactively $ (/) <$> PFR.React prod1 <*> PFR.React prod2

    prodRatio' :: P.Producer [D.Decimal] (StateT GTS.SignalModel STM) ()
    prodRatio' = prodRatio P.>-> PM.buffer 2 [] P.>-> PM.store id GTS.ratioOfSignals

    threshCrsd :: P.Pipe [D.Decimal] (Maybe GTS.CrossedDirection) STM ()
    threshCrsd = P.for P.cat $ \rs -> do
      t <- lift $ readTVar threshold
      let r = rs ^? ix 0
          prevR = rs ^? ix 1
      P.yield (GTS.thresholdCrossed (Just t) r prevR)

    threshCrsd' :: P.Producer (Maybe GTS.CrossedDirection) (StateT GTS.SignalModel STM) ()
    threshCrsd' = prodRatio' P.>-> hoist lift threshCrsd
      P.>-> PM.store id GTS.ratioThresholdCrossed

    initialSigModel = appModel ^. GTA.appSignalModel

    -- output signal
    prodSigModel :: P.Producer GTS.SignalModel STM ()
    prodSigModel = PL.evalStateP initialSigModel (threshCrsd' P.>-> PM.retrieve id)

    consumeSigModel :: P.Consumer GTS.SignalModel IO ()
    consumeSigModel = do
      -- await atomically, as it's impossible to await all values in one transaction
      a <- P.await
      b <- lift $ atomically $ PC.send outputUi (GTA.SetSignalModel . GTF.SetField $ a)
      if b
        then consumeSigModel
        else pure ()

  -- fork a thread that continously outputs prodSigModel to outputUi
  void . liftIO . C.forkIO . void . P.runEffect $ hoist atomically prodSigModel P.>-> consumeSigModel

  void $ startUi refreshDelay appModel outputUi inputUi (seal1 >> seal2 >> sealUi) threshold
