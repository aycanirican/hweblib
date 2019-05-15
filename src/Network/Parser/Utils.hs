
module Network.Parser.Utils
  where

--
-- Module      :  Network.Parser.Utils
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Common Utilities for Parsers

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Prelude hiding (take)
--------------------------------------------------------------------------------

-- * Common Parsing Utilities

-- | Parse `n` digit as `Int` validated by `p`
nDigitInt :: Int -> (Int -> Bool) -> Parser Int
nDigitInt n p = do
  xs <- count n digit
  let conv = sum . fmap (uncurry (*))
             . Prelude.zip [10^i| i <- Prelude.reverse [0..(n-1)]]
             . fmap digitToInt
      x    = conv xs
  if p x then return x else fail "Invalid Integer"

-- | Parse `p` at max `n` times
maxP :: Int -> Parser a -> Parser [a]
maxP 0 _ = return empty
maxP n p = ((:) <$> p <*> maxP (n-1) p) <|> return empty

-- | delimited parser (ex: fixed 100 char only matches char 100 times
-- maximum)
fixed :: Int -> Parser a -> Parser a
fixed i p = do
    intermediate <- take i
    case parseOnly (p <* endOfInput) intermediate of
        Left _ -> empty
        Right x -> return x
