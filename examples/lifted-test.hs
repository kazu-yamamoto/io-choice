{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Main where
import Control.Exception.IOChoice.Lifted.TH
import Control.Exception.Lifted
import Control.Monad.Writer
import Prelude hiding (ioError)
import Control.Monad.Trans.Control

(|||>) :: MonadBaseControl IO m => m a -> m a -> m a
(|||>) = $(newIOChoice [''ErrorCall, ''ArithException])

main :: IO ()
main = execWriterT $ do
  a0 <- evaluate (1 `div` 0)
   |||> return 3
  liftIO $ putStrLn $ "Should be 3: " ++ show a0
  a1 <- error "Unexpected answer!"
   |||> return "expected answer."
  liftIO $ putStrLn $ "This is an " ++ a1
  a2 <- ioError (userError "IO Error!")
   |||> return "IO Exception is handled by default."
  liftIO $ putStrLn a2
  a3 <- assert False (return "should be fail.")
   |||> return "this should not be seen."
  liftIO $ putStrLn $ "This message should not be printed: " ++ a3
