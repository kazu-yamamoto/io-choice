{-# LANGUAGE TemplateHaskell #-}

module IOChoiceTHSpec where

import Control.Exception
import Control.Exception.IOChoice
import Control.Exception.IOChoice.TH
import System.IO.Error
import Test.Hspec

(||>>) :: IO a -> IO a -> IO a
(||>>) = $(newIOChoice [''ErrorCall, ''ArithException])

spec :: Spec
spec = describe "||>>" $ do
    it "selects IO" $ do
        good ||>> bad `shouldReturn` "good"
        bad ||>> good `shouldReturn` "good"
    it "throws an error if all choices fail" $ do
        bad ||>> bad `shouldThrow` isUserError
    it "can be used with goNext" $ do
        goNext ||>> good `shouldReturn` "good"
    it "also catches ArithException" $ do
        throwIO Overflow ||>> good `shouldReturn` "good"
    it "does not catch non-specified exceptions" $ do
        throwIO Deadlock ||>> good `shouldThrow` isDeadlock

good :: IO String
good = return "good"

bad :: IO String
bad = throwIO ioErr

ioErr :: IOException
ioErr = userError "userError"

isOverLow :: Selector ArithException
isOverLow e = e == Overflow

isDeadlock :: Selector Deadlock
isDeadlock = const True
