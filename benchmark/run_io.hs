module Main where

import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           IO (nextInput, nextVector)

main :: IO ()
main = do
  n <- nextInput :: IO Int
  v <- nextVector n :: IO (Vector Int)
  print (V.sum v)
