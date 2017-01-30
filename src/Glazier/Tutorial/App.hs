{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glazier.Tutorial.App where

import Control.Lens
import qualified Data.Decimal as D
import qualified Data.Text as T
import qualified Glazier.Tutorial.Counter as GTC
import qualified Glazier.Tutorial.Field as GTF
import qualified Glazier.Tutorial.StreamModel as GTS

data AppAction =
  QuitAction
  | AppCounterAction GTC.CounterAction
  | AppMessageAction (GTF.FieldAction T.Text)
  | SetStreamModel (GTF.FieldAction GTS.StreamModel)

makeClassyPrisms ''AppAction

instance GTC.AsCounterAction AppAction where
  _CounterAction = _AppCounterAction

data AppModel = AppModel
    { appModelCounterModel :: GTC.CounterModel
    , appModelMessageModel :: T.Text
    , appModelStreamModel :: GTS.StreamModel
    } deriving (Show)

makeFields ''AppModel

instance GTS.HasSignal1 AppModel (Maybe D.Decimal) where
    signal1 = streamModel . GTS.signal1

instance GTS.HasSignal2 AppModel (Maybe D.Decimal) where
    signal2 = streamModel . GTS.signal2

instance GTS.HasRatioOfSignals AppModel [D.Decimal] where
    ratioOfSignals = streamModel . GTS.ratioOfSignals

instance GTS.HasRatioThresholdCrossed AppModel (Maybe GTS.CrossedDirection) where
    ratioThresholdCrossed = streamModel . GTS.ratioThresholdCrossed

class HasThreshold s a | s -> a where
    threshold :: Lens' s a

instance HasThreshold GTC.CounterModel D.Decimal where
    threshold f c = fmap (fst . properFraction) (f (D.Decimal 0 $ fromIntegral c))

instance HasThreshold AppModel D.Decimal where
    threshold = counterModel . threshold
