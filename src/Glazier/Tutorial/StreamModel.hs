{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.Tutorial.StreamModel where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Decimal as D
import Data.Maybe
import qualified Glazier.Gadget.Strict as G

data CrossedDirection = CrossedUpwards | CrossedDownwards
  deriving (Eq, Show)

thresholdCrossed :: Ord a => a -> a -> a -> Maybe CrossedDirection
thresholdCrossed threshold currentRatio previousRatio
  | currentRatio > threshold && previousRatio < threshold = Just CrossedUpwards
  | currentRatio < threshold && previousRatio > threshold = Just CrossedDownwards
  | otherwise = Nothing

data ThresholdCommand = ThresholdSet D.Decimal
  deriving (Eq, Show)

-- | Given a function to get the threshold from the current state,
-- return an Update function that will ignore the input action, and
-- return a ThresholdSet command
thresholdGadget :: Monad m => (s -> Maybe ThresholdCommand) -> G.Gadget s m a [ThresholdCommand]
thresholdGadget f = G.Gadget $ do
  s <- get
  pure (catMaybes [f s])

-- | Since reading from STM is side-effectful
-- We can't have separate streams reading from the same source
-- So to emulate multiple streams, we need to produce a tuple of stream
-- outputs.
-- The idea is then to use the classy lens pattern around this tuple to
-- make access and update logic clearer.
data StreamModel =
  StreamModel
  { streamModelSignal1               :: !(Maybe D.Decimal)
  , streamModelSignal2               :: !(Maybe D.Decimal)
  , streamModelRatioOfSignals        :: ![D.Decimal]
  , streamModelRatioThresholdCrossed :: !(Maybe CrossedDirection)
  , streamModelRatioThresholdCrossedPin :: {-# UNPACK #-} !D.Decimal -- When zero, RatioThresholdCrossed is allowed to be Nothing
  } deriving (Eq, Show)

makeFields ''StreamModel
