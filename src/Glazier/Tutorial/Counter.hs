{-# LANGUAGE TemplateHaskell #-}

module Glazier.Tutorial.Counter where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Glazier as G
import qualified Math.NumberTheory.Logarithms as Math

type CounterModel = Int

-- | NB. Increment and Decrement are not idempotent.
data CounterAction = ResetCount | Increment | Decrement | SetCount Int
  deriving (Eq, Show)

makeClassyPrisms ''CounterAction

data CounterCommand = UpperBoundsExceeded | LowerBoundsExceeded
  deriving (Enum, Eq, Show)

-- | NB. Because the amount to increment depends on the current state, it is importanat
-- that the CounterAction is just "Increment" as opposed to "IncrementBy Int".
-- Otherwise, if processing it held up, you may get multiple "IncrementBy LargeNumber"
-- which is not expected behaviour.
-- Alternatively, consider changing the Action to be idempotent (eg only @SetCount Int@).
notifyCounterButton :: Int -> G.Notify CounterAction CounterModel [CounterCommand]
notifyCounterButton b = G.Notify $ do
  a <- ask
  case a of
    ResetCount -> do
      put 0
      pure []
    Increment -> state $ limited b . increment
    Decrement -> state $ limited b . decrement
    SetCount n -> state . const $ limited b n

-- | Limit to a threshold
limited :: (Num a, Ord a) => a -> a -> ([CounterCommand], a)
limited b s
  | s > 0 =
    if b' > s
      then ([], s)
      else ([UpperBoundsExceeded], b')
  | s < 0 =
    if (-b') < s
      then ([], s)
      else ([LowerBoundsExceeded], -b')
  | otherwise = ([], 0)
 where
  b' = abs b

-- | Go up by 1 (up to 10), then go up by 10 (up to 100), etc
increment :: Integral a => a -> a
increment n = n + 10 ^ numDigits (nudge 1 n)

-- | Go down by 1 (up to -10), then go down by -10 (up to 100), etc
decrement :: Integral a => a -> a
decrement n = n - 10 ^ numDigits (nudge (-1) n)

-- | Number of digits
-- abs n < 9: 0
-- abs n < 99: 1
-- abs n < 999: 2
numDigits :: Integral a => a -> Int
numDigits 0 = 0
numDigits n = Math.integerLog10 $ abs $ fromIntegral n

-- | Used to nudge a number at boundary for numDigits.
-- Eg.
--   at 10: when incrementing, increase by 10, but when decrementing decrease by 1.
--   at 100: when incrementing, increase by 100, but when decrementing decrease by 10.
-- That is:
--   When moving away from zero, don't change the number of digits, ie.
--     if incrementing, and number is positive, no change
--     if decrementing, and number is negative, no change
--   When moving towards zero, reduce the number of digits at boundary, ie.
--     if incrementing, and number is negative, nudge + 1
--     if decrementing, and number is positive, nudge - 1
nudge :: (Eq a, Num a) => a -> a -> a
nudge sign' n =
  if signum n == signum sign'
    then n
    else n + signum sign'
