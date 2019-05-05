{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.7232
-- Copyright   :  Aycan iRiCAN 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Hypertext Transfer Protocol (HTTP/1.1): Conditional Requests
--
-- <http://www.ietf.org/rfc/rfc7232.txt>

module Network.Parser.Rfc7232 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString
import           Data.Time
import           Data.Word
--------------------------------------------------------------------------------
import           Network.Parser.Rfc5234           (dquote)
import           Network.Parser.Rfc7230
import           Network.Parser.Rfc7231
--------------------------------------------------------------------------------

{- Appendix C.  Collected ABNF

   In the collected ABNF below, list rules are expanded as per Section
   1.2 of [RFC7230].

   ETag = entity-tag

   HTTP-date = <HTTP-date, see [RFC7231], Section 7.1.1.1>

   If-Match = "*" / ( *( "," OWS ) entity-tag *( OWS "," [ OWS
    entity-tag ] ) )
   If-Modified-Since = HTTP-date
   If-None-Match = "*" / ( *( "," OWS ) entity-tag *( OWS "," [ OWS
    entity-tag ] ) )
   If-Unmodified-Since = HTTP-date

   Last-Modified = HTTP-date

   OWS = <OWS, see [RFC7230], Section 3.2.3>

   entity-tag = [ weak ] opaque-tag
   etagc = "!" / %x23-7E ; '#'-'~'
    / obs-text

   obs-text = <obs-text, see [RFC7230], Section 3.2.6>
   opaque-tag = DQUOTE *etagc DQUOTE

   weak = %x57.2F ; W/
-}

-- * 2.2.  Last-Modified
last_modified :: Parser UTCTime
last_modified = http_date

-- * 2.3.  ETag

-- >>> parseOnly etag "\"xyzzy\""
-- Right "\"xyzzy\""
-- >>> parseOnly etag "W/\"xyzzy\""
-- Right "W/\"xyzzy\""
-- >>> parseOnly etag "\"\""
-- Right "\"\""
etag :: Parser ByteString
etag = entity_tag

entity_tag :: Parser ByteString
entity_tag = (<>) <$> option mempty weak <*> opaque_tag

weak :: Parser ByteString
weak = AC.string "W/"

opaque_tag :: Parser ByteString
opaque_tag = ret <$> asList dquote
                 <*> many etagc
                 <*> asList dquote
  where ret a b c = pack (a ++ b ++ c)

etagc :: Parser Word8
etagc = satisfy (\w -> (w == 0x21) || (w >= 0x23 && w <= 0x7e)) <|> obs_text

-- * 3.1.  If-Match

-- >>> parseOnly if_match "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\""
--
if_match :: Parser (Either Star [ByteString])
if_match = A.eitherP (AC.char '*' *> pure Star) (dash1 entity_tag)

-- * 3.2.  If-None-Match
if_none_match :: Parser (Either Star [ByteString])
if_none_match = if_match

-- * 3.3.  If-Modified-Since
if_modified_since :: Parser UTCTime
if_modified_since = http_date

-- * 3.4.  If-Unmodified-Since
if_unmodified_since :: Parser UTCTime
if_unmodified_since = http_date

-- * 3.5.  If-Range
-- TODO: 7233

