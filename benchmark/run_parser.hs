module Main where

import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Parser

main :: IO ()
main = do
  contents <- B.getContents
  (print . sum . runParser contents) (untilEof (whitespace *> int <* whitespace))
