{-# LANGUAGE FlexibleContexts #-}

module IOChoiceLiftedSpec where

import Control.Exception.Lifted
import Control.Exception.IOChoice.Lifted
import System.IO.Error
import Test.Hspec
import Control.Monad.Trans.Identity

spec :: Spec
spec = describe "||>" $ do
    it "selects lifted IO" $ do
        runIdentityT (good ||> bad) `shouldReturn` "good"
        runIdentityT (bad ||> good) `shouldReturn` "good"
    it "throws an error if all choices fail" $ do
        runIdentityT (bad ||> bad) `shouldThrow` isUserError
    it "can be used with goNext" $ do
        runIdentityT (goNext ||> good) `shouldReturn` "good"
    it "does not catch exceptions except IOException" $ do
        runIdentityT (throwIO Overflow ||> good) `shouldThrow` isOverLow

good :: IdentityT IO String
good = return "good"

bad :: IdentityT IO String
bad = throwIO ioErr

ioErr :: IOException
ioErr = userError "userError"

isOverLow :: Selector ArithException
isOverLow e = e == Overflow
