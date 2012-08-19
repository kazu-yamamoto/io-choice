{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Control.Exception.IOChoice.Lifted.TH (newIOChoice) where
import Control.Exception.Lifted
import Language.Haskell.TH

import Control.Exception.IOChoice.THUtil

newIOChoice :: [Name] -> ExpQ
newIOChoice =  newChoice [| catches |] [| Handler |]
