{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

-- |
-- This package provides a function to generate a choice operator
-- in IO monad by specifying exceptions to be caught.

module Control.Exception.IOChoice.TH (newIOChoice) where

import Control.Exception
import Language.Haskell.TH

import Control.Exception.IOChoice.THUtil

-- |
-- A function to generate a choice operator in IO monad.
-- 'IOException' is automatically added to specified exceptions.
-- So, 'Control.Exception.IOChoice.goNext' can be used with
-- the new operator.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Control.Exception
-- > import Control.Exception.IOChoice.TH
-- >
-- > (||>>) :: IO a -> IO a -> IO a
-- > (||>>) = $(newIOChoice [''ErrorCall, ''ArithException])

newIOChoice :: [Name] -> ExpQ
newIOChoice =  newChoice [| catches |] [| Handler |]
