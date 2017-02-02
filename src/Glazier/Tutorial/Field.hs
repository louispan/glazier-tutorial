{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.Tutorial.Field where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Glazier.Gadget.Strict as G

-- | Basic widget which can be set to a new value
data FieldAction s = SetField s | ModifyField (s -> s)

makeClassyPrisms ''FieldAction

fieldGadget :: Monad m => G.Gadget s m (FieldAction s) [r]
fieldGadget = G.Gadget $ do
    a <- ask
    case a of
        SetField s -> put s
        ModifyField f -> modify f
    pure []
