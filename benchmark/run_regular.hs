module Main where

import qualified Data.ByteString.Char8 as B
import           Data.Maybe (fromJust)

main :: IO ()
main =
  print =<< sum . fmap (fst . fromJust . B.readInt) . B.words <$> B.getContents
