{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This package provides the choice operator ('||>') for
-- IO monad.

module Control.Exception.IOChoice where

import Control.Exception (IOException)
import qualified Control.Exception as E

-- |
-- If 'IOException' occurs or 'goNext' is used in the left IO,
-- then the right IO is performed. Note that 'fail'
-- throws 'IOException'.

(||>) :: IO a -> IO a -> IO a
x ||> y = x `E.catch` (\(_ :: IOException) -> y)

infixl 3 ||>

-- | Go to the next 'IO' monad by throwing 'IOException'.
goNext :: IO a
goNext = E.throwIO $ userError "goNext for IO"

-- | Run any one 'IO' monad.
runAnyOne :: [IO a] -> IO a
runAnyOne = foldr (||>) goNext
