{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

-- |
-- This package provides a function to generate a choice operator
-- in lifted IO monad by specifying exceptions to be caught.

module Control.Exception.IOChoice.Lifted.TH (newIOChoice) where

import Control.Exception.Lifted
import Language.Haskell.TH

import Control.Exception.IOChoice.THUtil

-- |
-- A function to generate a choice operator in lifted IO monad.
-- 'IOException' is automatically added to specified exceptions.
-- So, 'Control.Exception.IOChoice.Lifted.goNext' can be used with
-- the new operator.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Control.Exception
-- > import Control.Exception.IOChoice.Lifted.TH
-- >
-- > (||>>) :: MonadBaseControl IO m => m a -> m a -> m a
-- > (||>>) = $(newIOChoice [''ErrorCall, ''ArithException])

newIOChoice :: [Name] -> ExpQ
newIOChoice =  newChoice [| catches |] [| Handler |]
