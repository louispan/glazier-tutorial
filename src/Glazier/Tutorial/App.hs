{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glazier.Tutorial.App where

import Control.Lens
import qualified Data.Text as T
import qualified Glazier.Tutorial.Counter as GTC
import qualified Glazier.Tutorial.Field as GTF
import qualified Glazier.Tutorial.SignalModel as GTS

data AppAction =
    Redraw
  | Close
  | AppCounterAction GTC.CounterAction
  | AppMessageAction (GTF.FieldAction T.Text)
  | SetSignalModel (GTF.FieldAction GTS.SignalModel)

makeClassyPrisms ''AppAction

instance GTC.AsCounterAction AppAction where
  _CounterAction = _AppCounterAction

data AppModel =
  AppModel
  { _appCounterModel :: GTC.CounterModel
  , _appMessageModel :: T.Text
  , _appSignalModel  :: GTS.SignalModel
  }
  deriving Show

makeClassy ''AppModel
