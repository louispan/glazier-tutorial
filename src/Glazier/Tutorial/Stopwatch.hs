{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.Tutorial.Stopwatch where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import qualified GHC.Generics as G
import qualified Pipes as P
-- import qualified Pipes.Concurrent as PC
import qualified System.Clock as C
import Control.Monad.Extra as E

data Stopwatch = Stopwatch
    { stopwatchElapsedTime :: C.TimeSpec -- time since application epoch
    , stopwatchLapTime :: C.TimeSpec -- time since last frame
    } deriving (Eq, Show, G.Generic)
makeFields ''Stopwatch

-- run 'go' in between generating the stopwatch signal.
-- Eg, go can be (threadDelay 20) to generate a stopwatch signal every 20 micros.
-- FIXME: API not very good - want to let consumer do the diffTimeSpec
-- ie just send cpuTime, and let the reader batch and work out diffTimSe
stopwatchSignal :: P.Pipe () Stopwatch IO ()
stopwatchSignal = do
    a <- lift $ C.getTime C.ThreadCPUTime
    let noTime = C.diffTimeSpec a a
    P.yield $ Stopwatch noTime noTime
    P.await
    stopwatchSignal' a
  where
      stopwatchSignal' :: C.TimeSpec -> P.Pipe () Stopwatch IO ()
      stopwatchSignal' x = do
          y <- lift $ C.getTime C.ThreadCPUTime
          P.yield $ Stopwatch (C.diffTimeSpec y x) y
          P.await
          stopwatchSignal' y


clockSignal :: C.Clock -> P.Pipe () C.TimeSpec IO r
clockSignal clock = forever $ do
    E.unit P.await
    a <- lift $ C.getTime clock
    P.yield a


--       aaaaaaaaaab    cc
-- aaaaaAbbbbbB    ccccC
-- Trigger logic is:
-- initial send
-- then forever: if mailbox is empty, then threadDelay, then send

