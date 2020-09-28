{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.7232
-- Copyright   :  Aycan iRiCAN 2010-2020
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

import Control.Applicative (Alternative (many, (<|>)))
import Data.Attoparsec.ByteString as A
  ( Parser,
    eitherP,
    option,
    satisfy,
  )
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString (ByteString, pack)
import Data.Time (UTCTime)
import Data.Word (Word8)
import Network.Parser.Rfc5234 (dquote)
import Network.Parser.Rfc7230 (obsText, asList, dash1)
import Network.Parser.Rfc7231 (httpDate, Star (..))
import Data.Functor (($>))

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
lastModified :: Parser UTCTime
lastModified = httpDate

-- * 2.3.  ETag

-- >>> parseOnly etag "\"xyzzy\""
-- Right "\"xyzzy\""
-- >>> parseOnly etag "W/\"xyzzy\""
-- Right "W/\"xyzzy\""
-- >>> parseOnly etag "\"\""
-- Right "\"\""
etag :: Parser ByteString
etag = entityTag

entityTag :: Parser ByteString
entityTag = (<>) <$> option mempty weak <*> opaqueTag

weak :: Parser ByteString
weak = AC.string "W/"

opaqueTag :: Parser ByteString
opaqueTag = ret <$> asList dquote
                <*> many etagc
                <*> asList dquote
  where 
     ret a b c = pack (a ++ b ++ c)

etagc :: Parser Word8
etagc = satisfy (\w -> (w == 0x21) || (w >= 0x23 && w <= 0x7e)) <|> obsText

-- * 3.1.  If-Match

-- >>> parseOnly ifMatch "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\""
--
ifMatch :: Parser (Either Star [ByteString])
ifMatch = A.eitherP (AC.char '*' $> Star) (dash1 entityTag)

-- * 3.2.  If-None-Match
ifNoneMatch :: Parser (Either Star [ByteString])
ifNoneMatch = ifMatch

-- * 3.3.  If-Modified-Since
ifModifiedSince :: Parser UTCTime
ifModifiedSince = httpDate

-- * 3.4.  If-Unmodified-Since
ifUnmodifiedSince :: Parser UTCTime
ifUnmodifiedSince = httpDate

-- * 3.5.  If-Range
-- TODO: 7233

