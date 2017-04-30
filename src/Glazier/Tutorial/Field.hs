{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.Tutorial.Field where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Glazier.Gadget as G

-- | Basic widget which can be set to a new value
data FieldAction s = SetField s | ModifyField (s -> s)

makeClassyPrisms ''FieldAction

fieldGadget :: Monad m => G.GadgetT e (FieldAction s) s m [c]
fieldGadget = G.GadgetT $ do
    a <- ask
    case a of
        SetField s -> put s
        ModifyField f -> modify f
    pure []
