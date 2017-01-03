{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glazier.Tutorial.App where

import Control.Lens
import qualified Data.Text as T
import qualified Glazier.Tutorial.Counter as GTC
import qualified Glazier.Tutorial.Field as GTF
import qualified Glazier.Tutorial.StreamModel as GTS

data AppAction =
    Redraw
  | Quit
  | AppCounterAction GTC.CounterAction
  | AppMessageAction (GTF.FieldAction T.Text)
  | SetStreamModel (GTF.FieldAction GTS.StreamModel)

makeClassyPrisms ''AppAction

instance GTC.AsCounterAction AppAction where
  _CounterAction = _AppCounterAction

data AppModel =
  AppModel
  { appModelCounterModel :: GTC.CounterModel
  , appModelMessageModel :: T.Text
  , appModelStreamModel  :: GTS.StreamModel
  }
  deriving Show

makeFields ''AppModel
