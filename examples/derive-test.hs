{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Exception.IOChoice.TH
import Control.Exception

(|||>) :: IO a -> IO a -> IO a
(|||>) = $(newIOChoice [''ErrorCall, ''ArithException])

main :: IO ()
main = do
  a0 <- evaluate (1 `div` 0)
   |||> return 3
  putStrLn $ "Should be 3: " ++ show a0
  a1 <- error "Unexpected answer!"
   |||> return "expected answer."
  putStrLn $ "This is an " ++ a1
  a2 <- ioError (userError "IO Error!")
   |||> return "IO Exception is handled by default."
  putStrLn a2
  a3 <- assert False (return "should be fail.")
   |||> return "this should not be seen."
  putStrLn $ "This message should not be printed: " ++ a3
