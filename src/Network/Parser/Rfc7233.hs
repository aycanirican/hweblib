{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module      :  Network.Parser.7233
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Hypertext Transfer Protocol (HTTP/1.1): Range Requests
--
-- <http://www.ietf.org/rfc/rfc7233.txt>

module Network.Parser.Rfc7233 where

import Control.Applicative
  ( Alternative (many, (<|>)),
    optional,
  )
import Data.Attoparsec.ByteString as BS
  ( Parser,
    eitherP,
    many1,
  )
import Data.Attoparsec.ByteString.Char8 as AC
  ( char,
    digit,
    string,
  )
import Data.ByteString (ByteString, pack)
import Data.Functor (($>))
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import qualified GHC.Generics as GHC
import qualified Network.Parser.Rfc2234 as R2234
import Network.Parser.Rfc7230 (dash1, token)
import Network.Parser.Rfc7231 (httpDate)
import Network.Parser.Rfc7232 (entityTag)
import Prelude hiding (product)


{- Holy memory consumer

   o  The first and last bytes only (bytes 0 and 9999):

        bytes=0-0,-1
-}

data ByteRange
  = ByteRange  (Maybe Int, Maybe Int)
  | ByteSuffixRange Int
    deriving (Eq, Show, GHC.Generic)
-- * 2.  Range Units

rangeUnit :: Parser ByteString
rangeUnit = bytesUnit <|> otherRangeUnit

-- * 2.1.  Byte Ranges
bytesUnit :: Parser ByteString
bytesUnit = AC.string "bytes"

byteRangesSpecifier :: Parser [ByteRange]
byteRangesSpecifier = (bytesUnit <* AC.char '=') *> byteRangeSet


byteRangeSet :: Parser [ByteRange]
byteRangeSet = dash1 ((ByteRange           <$> byteRangeSpec)
                  <|> (ByteSuffixRange <$> suffixByteRangeSpec))

byteRangeSpec :: Parser (Maybe Int, Maybe Int)
byteRangeSpec = (,) <$> (Just <$> (firstBytePos <* AC.char '-'))
                    <*> optional lastBytePos

firstBytePos :: Parser Int
firstBytePos = read <$> many1 digit

lastBytePos :: Parser Int
lastBytePos = read <$> many1 digit

suffixByteRangeSpec :: Parser Int
suffixByteRangeSpec = AC.char '-' *> suffixLength

suffixLength :: Parser Int
suffixLength = read <$> many1 digit

-- * 2.2.  Other Range Units
otherRangeUnit :: Parser ByteString
otherRangeUnit = token

-- * 2.3.  Accept-Ranges
acceptableRanges :: Parser [ByteString]
acceptableRanges = dash1 rangeUnit

-- * 3.  Range Requests
-- * 3.1.  Range

{-
Range = byte-ranges-specifier / other-ranges-specifier
other-ranges-specifier = other-range-unit "=" other-range-set
other-range-set = 1*VCHAR
-}

range :: Parser (Either [ByteRange] (ByteString, ByteString))
range = eitherP byteRangesSpecifier otherRangesSpecifier

otherRangesSpecifier :: Parser (ByteString, ByteString)
otherRangesSpecifier = (,) <$> otherRangeUnit <*> ("=" *> otherRangeSet)

otherRangeSet :: Parser ByteString
otherRangeSet = pack <$> many1 R2234.vchar

-- * 3.2.  If-Range

{-
If-Range = entity-tag / HTTP-date
-}
ifRange :: Parser (Either ByteString UTCTime)
ifRange = (Left <$> entityTag) <|> (Right <$> httpDate)

-- * 4.2.  Content-Range

{-
Content-Range       = byte-content-range
                      / other-content-range

byte-content-range  = bytes-unit SP
                      ( byte-range-resp / unsatisfied-range )

byte-range-resp     = byte-range "/" ( complete-length / "*" )
byte-range          = first-byte-pos "-" last-byte-pos
unsatisfied-range   = "*/" complete-length

complete-length     = 1*DIGIT

other-content-range = other-range-unit SP other-range-resp
other-range-resp    = *CHAR
-}

-- contentRange :: ByteString, Either ((Int, Int), Maybe Int) Int)
-- contentRange = eitherP byteContentRange otherContentRange

data ContentRange = ContentRangeSatisfied (Int, Int) ContentRangeLength
                  | ContentRangeUnsatisfied Int
    deriving (Eq, Show, Typeable, GHC.Generic)

data ContentRangeLength = ContentRangeLengthKnown Int
                        | ContentRangeLengthUnknown
    deriving (Eq, Show, Typeable, GHC.Generic)

contentRange :: Parser (Either ContentRange (ByteString, ByteString))
contentRange = eitherP byteContentRange otherContentRange

byteContentRange :: Parser ContentRange
byteContentRange = byteRangeResp <|> unsatisfiedRange

byteRangeResp :: Parser ContentRange
byteRangeResp
  = ContentRangeSatisfied
      <$> (byteRange <* AC.char '/')
      <*> (    (ContentRangeLengthKnown <$> completeLength)
           <|> (AC.char '*' $> ContentRangeLengthUnknown))

byteRange :: Parser (Int, Int)
byteRange = (,) <$> firstBytePos <*> (AC.char '-' *> lastBytePos)

unsatisfiedRange :: Parser ContentRange
unsatisfiedRange = ContentRangeUnsatisfied <$> ("*/" *> completeLength)

completeLength :: Parser Int
completeLength = read <$> many1 digit

otherContentRange :: Parser (ByteString, ByteString)
otherContentRange = (,) <$> otherRangeUnit <*> (R2234.sp *> otherRangeResp)

otherRangeResp :: Parser ByteString
otherRangeResp = pack <$> many R2234.octet
