{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.Tutorial.Message where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Glazier as G

-- | Basic widget which can be set to a new value
data MessageAction s = ClearMessageIf (s -> Bool) | ModifyMessage (s -> s)

makeClassyPrisms ''MessageAction

notifyMessage :: G.Notify (MessageAction s) s [r]
notifyMessage = G.Notify $ do
  a <- ask
  s <- get
  case a of
    ClearMessageIf p -> 
      put a'
      pure []
