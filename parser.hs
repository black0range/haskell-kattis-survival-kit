{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-|
Module      : Parser
Description : Bytestring parser for problem solving on Kattis
Authors     : Isaac Arvestad and Tomas Möre

-}
module Parser
  ( Parser
  , runParser
  , peekEof
  , peek
  , untilEof
  , untilP
  , char
  , manyP
  , skip
  , whitespace
  , int
  ) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as UB
import           Data.Char
import           Data.Maybe
import           Data.STRef
import           Foreign.C.String (CString)
import           System.IO.Unsafe (unsafePerformIO)

-- | A bytestring pointer keeps track of a reference to a bytestring as well as
-- an index of where to look in the bytestring.
data BSPtr s = BSPtr
  { ref :: {-# UNPACK #-} !(STRef s ByteString)
  , pos :: {-# UNPACK #-} !Int
  }

-- | A parser with ByteString input.
type Parser s a = ParserS s ByteString a

-- | A parser parameterised over state 's', input 'i' and result 'a'.
newtype ParserS s i a = ParserS
  { run :: forall s. BSPtr s -> ST s (a, BSPtr s)
  }

instance Functor (ParserS s i) where
  fmap f k = ParserS $ \bsPtr -> first f <$> run k bsPtr
  {-# INLINE fmap #-}

instance Applicative (ParserS s i) where
  pure x = ParserS $ \bsPtr -> pure (x, bsPtr)
  {-# INLINE pure #-}

  (ParserS k1) <*> (ParserS k2) =
    ParserS $ \bsPtr -> do
      (f, bsPtr') <- k1 bsPtr
      (x, bsPtr'') <- k2 bsPtr'
      pure (f x, bsPtr'')
  {-# INLINE (<*>) #-}

instance Monad (ParserS s i) where
  (ParserS k1) >>= f =
    ParserS $ \bsPtr -> do
      (x, bsPtr') <- k1 bsPtr
      run (f x) bsPtr'
  {-# INLINE (>>=) #-}

-- | Run parser with a bytestring as the input stream.
runParser :: forall s a. ByteString -> ParserS s ByteString a -> a
runParser bs kattio =
  runST $ do
    bsRef <- newSTRef bs
    fst <$> run kattio (BSPtr bsRef 0)

-- | Offsets the position in a bytestring pointer by 'i' steps.
next :: BSPtr s -> Int -> BSPtr s
next !(BSPtr r p) !i = BSPtr r (p + i)
{-# INLINE next #-}

-- | Returns true if at end of input stream. Does not parse any characters.
peekEof :: ParserS s ByteString Bool
peekEof =
  ParserS $ \bsPtr -> do
    l <- B.length <$> readSTRef (ref bsPtr)
    pure (pos bsPtr >= l, bsPtr)
{-# INLINE peekEof #-}

-- | Look at the next character without parsing it.
peek :: ParserS s ByteString Char
peek =
  ParserS $ \bsPtr -> do
    c <- flip B.index (pos bsPtr) <$> readSTRef (ref bsPtr)
    pure (c, bsPtr)
{-# INLINE peek #-}

-- | Runs a parser 'p' until eof is hit and returns the list of parsed results.
untilEof :: ParserS s ByteString a -> ParserS s ByteString [a]
untilEof p = do
  b <- peekEof
  if b
    then pure []
    else do
      x <- p
      (x :) <$> untilEof p
{-# INLINE untilEof #-}

-- | Runs a parser 'p' until a character is encountered which fulfills a
-- predicate. This character is not parsed.
untilP :: ParserS s ByteString a -> (Char -> Bool) -> ParserS s ByteString [a]
untilP parser predicate = do
  c <- peek
  if predicate c
    then pure []
    else do
      x <- parser
      (x :) <$> untilP parser predicate
{-# INLINE untilP #-}

-- | Parses characters into a bytestring as long as a predicate 'p' holds.
manyP :: (Char -> Bool) -> ParserS s ByteString ByteString
manyP p =
  ParserS $ \bsPtr -> do
    w <- B.takeWhile p . UB.unsafeDrop (pos bsPtr) <$> readSTRef (ref bsPtr)
    pure (w, next bsPtr (B.length w))
{-# INLINE manyP #-}

-- | Skips characters as long as a predicate 'p' holds.
skip :: (Char -> Bool) -> ParserS s ByteString ()
skip p = manyP p *> pure ()
{-# INLINE skip #-}

-- | Parses any character.
char :: ParserS s ByteString Char
char =
  ParserS $ \bsPtr -> do
    c <- flip B.index (pos bsPtr) <$> readSTRef (ref bsPtr)
    pure (c, next bsPtr 1)
{-# INLINE char #-}

-- | Skips any whitespace characters.
whitespace :: ParserS s ByteString ()
whitespace = skip isSpace
{-# INLINE whitespace #-}

-- | Parses an integer.
int :: ParserS s ByteString Int
int = readInt <$> manyP isDigit
{-# INLINE int #-}

-- | atoi converts a string to an int.
foreign import ccall unsafe "atoi" c_atoi :: CString -> IO Int

-- | atoi converts a string to an double.
foreign import ccall unsafe "atof" c_atof :: CString -> IO Double

-- | Read an integer from a bytestring using an unsafe FFI call to 'atoi'.
readInt :: ByteString -> Int
readInt s = unsafePerformIO $ B.useAsCString s c_atoi
{-# INLINE readInt #-}

-- | Read a double from a bytestring using an unsafe FFI call to 'atof'.
readDouble :: ByteString -> Double
readDouble s = unsafePerformIO $ B.useAsCString s c_atof
{-# INLINE readDouble #-}
