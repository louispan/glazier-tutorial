{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.Tutorial.Field where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Glazier as G

-- | Basic widget which can be set to a new value
newtype FieldAction a = SetField a

-- | can't makeClassyPrisms for single constructor
class AsFieldAction s a | s -> a where
  _FieldAction :: Prism' s (FieldAction a)
  _SetField :: Prism' s a
  _SetField = _FieldAction . _SetField

instance AsFieldAction (FieldAction a) a where
  _FieldAction = id
  _SetField = prism
            SetField
            (\(SetField a) -> Right a)

notifyField :: G.Notify (FieldAction a) a [r]
notifyField = G.Notify $ do
  a <- ask
  case a of
    SetField a' -> do
      put a'
      pure []
