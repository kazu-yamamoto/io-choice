{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- |
-- This package provides the choice operator ('||>') for
-- lifted IO monad.

module Control.Exception.IOChoice.Lifted where

import Control.Exception (IOException)
import qualified Control.Exception.Lifted as L (catch, throwIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)

-- |
-- If 'IOException' occurs or 'goNext' is used in the left monad,
-- then the right monad is performed. Note that 'fail'
-- throws 'IOException'.

(||>) :: MonadBaseControl IO m => m a -> m a -> m a
x ||> y = x `L.catch` (\(_ :: IOException) -> y)

infixr 3 ||>

-- | Go to the next 'IO' monad by throwing 'IOException'.
goNext :: (MonadIO m, MonadBase IO m) => m a
goNext = L.throwIO $ userError "goNext for lifted IO"

-- | Run any one lifted 'IO' monad.
runAnyOne :: (MonadIO m, MonadBaseControl IO m) => [m a] -> m a
runAnyOne = foldr (||>) goNext
