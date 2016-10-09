module Glazier.Tutorial.IO where

import qualified Control.Concurrent as C
import Control.Monad.IO.Class

-- | action then delay
intermittently :: MonadIO m => Int -> m a -> m a
intermittently i action = do
  a <- action
  liftIO $ C.threadDelay i
  pure a
