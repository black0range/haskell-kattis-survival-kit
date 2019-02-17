module Main where

import Control.Monad
import Data.List
import System.Environment
import System.Random
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case fmap read args of
    [n] -> do
      xs <- replicateM n $ randomRIO (0, 10 ^ 6)
      (putStrLn . unlines . fmap show) (n : xs)
    _ -> error "Expected a single argument: number of integers to generate"
