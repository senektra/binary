module Main (main) where

import Control.Concurrent
import qualified Control.Exception as E
import System.Exit
import System.IO
import System.Posix.Signals
import System.Random

binary :: IO ()
binary = do
  tid <- myThreadId
  _ <- installHandler keyboardSignal (Catch (E.throwTo tid ExitSuccess)) Nothing
  hSetBuffering stdout NoBuffering
  n <- randomRIO (1, 140) :: IO Int
  writeBinary n
  putStr "\n"
  binary
  where
    writeBinary n
      | n == 0 = return ()
      | otherwise = do
          b <- randomRIO (0, 1) :: IO Int
          putStr $ show b
          threadDelay 160000
          writeBinary (n - 1)

main :: IO ()
main = binary
