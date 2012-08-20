{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module IOChoiceLiftedTHSpec where

import Control.Exception.Lifted
import Control.Exception.IOChoice.Lifted
import Control.Exception.IOChoice.Lifted.TH
import System.IO.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Control
import Test.Hspec

(||>>) :: MonadBaseControl IO m => m a -> m a -> m a
(||>>) = $(newIOChoice [''ErrorCall, ''ArithException])

spec :: Spec
spec = describe "||>>" $ do
    it "selects lifted IO" $ do
        runIdentityT (good ||>> bad) `shouldReturn` "good"
        runIdentityT (bad ||>> good) `shouldReturn` "good"
    it "throws an error if all choices fail" $ do
        runIdentityT (bad ||>> bad) `shouldThrow` isUserError
    it "can be used with goNext" $ do
        runIdentityT (goNext ||>> good) `shouldReturn` "good"
    it "also catches ArithException" $ do
        runIdentityT (throwIO Overflow ||>> good) `shouldReturn` "good"
    it "does not catch non-specified exceptions" $ do
        runIdentityT (throwIO Deadlock ||>> good) `shouldThrow` isDeadlock

good :: IdentityT IO String
good = return "good"

bad :: IdentityT IO String
bad = throwIO ioErr

ioErr :: IOException
ioErr = userError "userError"

isOverLow :: Selector ArithException
isOverLow e = e == Overflow

isDeadlock :: Selector Deadlock
isDeadlock = const True
