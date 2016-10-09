{-# LANGUAGE FlexibleContexts #-}

-- | Module for generating a randomly fluctuating signal.
module Glazier.Tutorial.Random where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Decimal as D

decimalIsoDouble :: Iso' D.Decimal Double
decimalIsoDouble = iso (fromRational . toRational) (fromRational . toRational)

-- | Randomly generate a new value that is randomly [-tick, tick] away from current.
-- The chance of +/-tick is 100% if current is 1 threshold away from desired.
generateTick :: MonadRandom m =>
     D.Decimal
  -> D.Decimal
  -> D.Decimal
  -> D.Decimal
  -> m D.Decimal
generateTick
  tick
  desired
  threshold
  currentValue =
  (D.roundTo 2 . view (from decimalIsoDouble))
  <$> getRandomR
  ( view decimalIsoDouble lowerB
  , view decimalIsoDouble upperB)
 where
  t' = abs tick
  threshold' = abs threshold
  lowerB =
    if currentValue >= desired
    then negate t'
    else modulate (desired - currentValue) threshold'
  upperB =
    if currentValue <= desired
    then t'
    else modulate (currentValue - desired) threshold'
  -- return a number between [-1.0, 1.0]
  modulate n dn = min 2.0 (n / (dn / 0.5)) - 1.0

randomValue :: (MonadState D.Decimal m, MonadRandom m) =>
     D.Decimal
  -> D.Decimal
  -> D.Decimal
  -> m ()
randomValue tick desired threshold = do
  s <- get
  t <- generateTick tick desired threshold s
  put $ s + t

randomSignal ::
  (MonadIO m, MonadState D.Decimal m, MonadRandom m) =>
  D.Decimal
  -> D.Decimal
  -> D.Decimal
  -> (Int, Int)
  -> (D.Decimal -> S.STM Bool)
  -> MaybeT m ()
randomSignal tick desired threshold interval sendVal =
  MaybeT $
    randomly interval $ do
      randomValue tick desired threshold
      s <- get
      b <- liftIO $ S.atomically $ sendVal s
      pure $ guard b

randomly :: (MonadRandom m, MonadIO m) => (Int, Int) -> m a -> m a
randomly interval action = do
  i <- getRandomR interval
  liftIO $ C.threadDelay i
  action
