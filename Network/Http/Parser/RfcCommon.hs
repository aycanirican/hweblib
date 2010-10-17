{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  , DeriveDataTypeable
  , TupleSections
  #-}

module Network.Http.Parser.RfcCommon where

import Control.Applicative hiding (many)
import Data.Attoparsec as AW
import Data.Attoparsec.Combinator as DACO
import Data.Attoparsec.Char8 as AC hiding (digit, char)
import qualified Data.Attoparsec.Char8 as DAC
import qualified Data.Attoparsec.FastSet as AF
import Data.ByteString as W
import Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8, Word64)
import Data.Char (digitToInt, isAsciiUpper, isAsciiLower)
import Control.Monad (join)
import Prelude hiding (take, takeWhile)
import Data.Typeable (Typeable)
import Data.Data (Data)


upalpha_pred, loalpha_pred, alpha_pred, digit_pred, hex_pred
 :: Word8 -> Bool

upalpha_pred w = w >= 65 && w <= 90
upalpha :: Parser Word8
upalpha = AW.satisfy upalpha_pred
{-# INLINE upalpha #-}

loalpha_pred w = w >= 97 && w <= 122
loalpha :: Parser Word8
loalpha = AW.satisfy loalpha_pred
{-# INLINE loalpha #-}

alpha_pred w = upalpha_pred w || loalpha_pred w
alpha :: Parser Word8
alpha = AW.satisfy alpha_pred
{-# INLINE alpha #-}

digit_pred w = w >= 48 && w <= 57 
digit :: Parser Word8
digit = AW.satisfy digit_pred
{-# INLINE digit #-}

hex_pred w = (w >= 65 && w <= 70) 
             || (w >= 97 && w <= 102)
             || (w >= 48 && w <= 57)
hex :: Parser [Word8]
hex = many1 $ AW.satisfy hex_pred
{-# INLINE hex #-}

-- | Utilities
appcon :: [a] -> [[a]] -> [a]
appcon = (. join) . (++)
word8l w = (:[]) <$> word8 w
toRepr = C.unpack . W.pack
