{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Control.Exception.IOChoice.TH (newIOChoice) where
import Control.Exception
import Language.Haskell.TH

import Control.Exception.IOChoice.THUtil

newIOChoice :: [Name] -> ExpQ
newIOChoice =  newChoice [| catches |] [| Handler |]
