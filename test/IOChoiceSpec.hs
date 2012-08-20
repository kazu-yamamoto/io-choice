module IOChoiceSpec where

import Control.Exception
import Control.Exception.IOChoice
import System.IO.Error
import Test.Hspec

spec :: Spec
spec = describe "||>" $ do
    it "selects IO" $ do
        good ||> bad `shouldReturn` "good"
        bad ||> good `shouldReturn` "good"
    it "throws an error if all choices fail" $ do
        bad ||> bad `shouldThrow` isUserError
    it "can be used with goNext" $ do
        goNext ||> good `shouldReturn` "good"
    it "does not catch exceptions except IOException" $ do
        throwIO Overflow ||> good `shouldThrow` isOverLow

good :: IO String
good = return "good"

bad :: IO String
bad = throwIO ioErr

ioErr :: IOException
ioErr = userError "userError"

isOverLow :: Selector ArithException
isOverLow e = e == Overflow
