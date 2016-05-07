{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

-- |
-- Module      :  Network.Parser.7233
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2015
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
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Attoparsec.ByteString       as BS
import           Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString
import           Data.Time
import           Data.Typeable
import qualified GHC.Generics                     as GHC
import           Prelude                          hiding (product)
--------------------------------------------------------------------------------
import qualified Network.Parser.Rfc2234           as R2234
import           Network.Parser.Rfc7230
import           Network.Parser.Rfc7231           (http_date)
import           Network.Parser.Rfc7232           (entity_tag)
--------------------------------------------------------------------------------

{- Holy memory consumer

   o  The first and last bytes only (bytes 0 and 9999):

        bytes=0-0,-1
-}

data ByteRange
  = ByteRange  (Maybe Int, Maybe Int)
  | ByteSuffixRange Int
    deriving (Eq, Show, GHC.Generic)
-- * 2.  Range Units

range_unit :: Parser ByteString
range_unit = bytes_unit <|> other_range_unit

-- * 2.1.  Byte Ranges
bytes_unit :: Parser ByteString
bytes_unit = AC.string "bytes"

byte_ranges_specifier :: Parser [ByteRange]
byte_ranges_specifier = (bytes_unit <* AC.char '=') *> byte_range_set


byte_range_set :: Parser [ByteRange]
byte_range_set = dash1 ((ByteRange           <$> byte_range_spec)
                        <|> (ByteSuffixRange <$> suffix_byte_range_spec))

byte_range_spec :: Parser (Maybe Int, Maybe Int)
byte_range_spec = (,) <$> (Just <$> (first_byte_pos <* AC.char '-'))
                      <*> option Nothing (Just <$> last_byte_pos)

first_byte_pos :: Parser Int
first_byte_pos = read <$> many1 digit

last_byte_pos :: Parser Int
last_byte_pos = read <$> many1 digit

suffix_byte_range_spec :: Parser Int
suffix_byte_range_spec = AC.char '-' *> suffix_length

suffix_length :: Parser Int
suffix_length = read <$> many1 digit

-- * 2.2.  Other Range Units
other_range_unit :: Parser ByteString
other_range_unit = token

-- * 2.3.  Accept-Ranges
acceptable_ranges :: Parser [ByteString]
acceptable_ranges = dash1 range_unit

-- * 3.  Range Requests
-- * 3.1.  Range

{-
Range = byte-ranges-specifier / other-ranges-specifier
other-ranges-specifier = other-range-unit "=" other-range-set
other-range-set = 1*VCHAR
-}

range :: Parser (Either [ByteRange] (ByteString, ByteString))
range = eitherP byte_ranges_specifier other_ranges_specifier

other_ranges_specifier :: Parser (ByteString, ByteString)
other_ranges_specifier = (,) <$> other_range_unit <*> ("=" *> other_range_set)

other_range_set :: Parser ByteString
other_range_set = pack <$> many1 R2234.vchar

-- * 3.2.  If-Range

{-
If-Range = entity-tag / HTTP-date
-}
if_range :: Parser (Either ByteString UTCTime)
if_range = (Left <$> entity_tag) <|> (Right <$> http_date)

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

-- content_range :: ByteString, Either ((Int, Int), Maybe Int) Int)
-- content_range = eitherP byte_content_range other_content_range

data ContentRange = ContentRangeSatisfied (Int, Int) ContentRangeLength
                  | ContentRangeUnsatisfied Int
    deriving (Eq, Show, Typeable, GHC.Generic)

data ContentRangeLength = ContentRangeLengthKnown Int
                        | ContentRangeLengthUnknown
    deriving (Eq, Show, Typeable, GHC.Generic)

content_range :: Parser (Either ContentRange (ByteString, ByteString))
content_range = eitherP byte_content_range other_content_range

byte_content_range :: Parser ContentRange
byte_content_range = byte_range_resp <|> unsatisfied_range

byte_range_resp :: Parser ContentRange
byte_range_resp
  = ContentRangeSatisfied
      <$> (byte_range <* AC.char '/')
      <*> (    (ContentRangeLengthKnown <$> complete_length)
           <|> (AC.char '*' *> return ContentRangeLengthUnknown))

byte_range :: Parser (Int, Int)
byte_range = (,) <$> first_byte_pos <*> (AC.char '-' *> last_byte_pos)

unsatisfied_range :: Parser ContentRange
unsatisfied_range = ContentRangeUnsatisfied <$> ("*/" *> complete_length)

complete_length :: Parser Int
complete_length = read <$> many1 digit

other_content_range :: Parser (ByteString, ByteString)
other_content_range = (,) <$> other_range_unit <*> (R2234.sp *> other_range_resp)

other_range_resp :: Parser ByteString
other_range_resp = pack <$> many R2234.octet
