{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

-- |
-- Module      :  Network.Parser.7231
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content
--
-- <http://www.ietf.org/rfc/rfc7231.txt>

module Network.Parser.Rfc7231 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Attoparsec.ByteString       as BS
import           Data.Attoparsec.ByteString.Char8 as AC
import           Data.Attoparsec.Combinator
import           Data.ByteString
import           Data.Char                        (digitToInt)
import           Data.Monoid
import           Data.Scientific
import           Data.Time
import           Data.Typeable
import qualified GHC.Generics                     as GHC
import           Prelude                          hiding (product)
--------------------------------------------------------------------------------
import qualified Network.Parser.Rfc3986           as R3986
import qualified Network.Parser.Rfc4647           as R4647
import           Network.Parser.Rfc5234
import qualified Network.Parser.Rfc5322           as R5322
import qualified Network.Parser.Rfc5646           as R5646
import           Network.Parser.Rfc7230
import           Network.Parser.Utils
import           Network.Types
-- import           Network.Parser.Mime
-- import qualified Network.Parser.Rfc2183           as R2183
-- import           Network.Parser.Rfc2822           (comment, msg_id, text)
-- import           Network.Parser.RfcCommon         hiding (text)
--------------------------------------------------------------------------------

-- | 3.1.  Representation Metadata

-- | 3.1.1.1. Media Type
{-
     media-type = type "/" subtype *( OWS ";" OWS parameter )
     type       = token
     subtype    = token
     parameter      = token "=" ( token / quoted-string )
-}

data MediaType
  = MediaType ByteString ByteString [(ByteString, ByteString)]
    deriving (Eq,Show,GHC.Generic,Typeable)

-- |
-- >>> parseOnly media_type "multipart/form-data; boundary=------------------------d380791e8587bb9a"
-- Right (MediaType "multipart" "form-data" [("boundary","------------------------d380791e8587bb9a")]
media_type = MediaType <$> type_ <* AC.char '/'
             <*> subtype
             <*> many (ows *> AC.char ';' *> ows *> parameter)
type_ = token
subtype = token

parameter :: Parser (ByteString, ByteString)
parameter = (,) <$> (token <* AC.char '=') <*> token

-- | 3.1.1.2. Charset
charset :: Parser ByteString
charset = token

-- | 3.1.1.5. Content Type
{-
     Content-Type = media-type
-}

content_type :: Parser MediaType
content_type = media_type

-- * 3.1.2. Encoding for Compression or Integrity
-- * 3.1.2.1. Content Codings

content_coding :: Parser ByteString
content_coding = token

-- * 3.1.2.2.  Content-Encoding
content_encoding = dash1 content_coding


-- * 3.1.3.  Audience Language

-- * 3.1.3.1.  Language Tags
language_tag :: Parser ByteString
language_tag = R5646.language_tag

-- * 3.1.3.2.  Content-Language

{-
Content-Language = 1#language-tag
-}

content_language :: Parser [ByteString]
content_language = dash1 language_tag

-- * 3.1.4.  Identification

-- * 3.1.4.2.  Content-Location

{-
Content-Location = absolute-URI / partial-URI
-}

-- TODO: add partial_uri
content_location = R3986.absoluteUri -- <|> partial-URI


-- * 5.1.1.  Expect

expect :: Parser ByteString
expect = string "100-continue"

-- * 5.1.2.  Max-Forwards

maxForwards :: Parser Int
maxForwards = read <$> many AC.digit

-- * 5.3.  Content Negotiation

-- * 5.3.1.  Quality Values

{-
weight = OWS ";" OWS "q=" qvalue
qvalue = ( "0" [ "." 0*3DIGIT ] )
       / ( "1" [ "." 0*3("0") ] )
-}

weight :: Parser Scientific
weight = ows *> ";" *> ows *> "q=" *> qvalue

qvalue :: Parser Scientific
qvalue = rank

-- * 5.3.2.  Accept

{-
Accept = #( media-range [ accept-params ] )
media-range    = ( "*/*"
                 / ( type "/" "*" )
                 / ( type "/" subtype )
                 ) *( OWS ";" OWS parameter )
accept-params  = weight *( accept-ext )
accept-ext = OWS ";" OWS token [ "=" ( token / quoted-string ) ]
-}


accept :: Parser [MediaType]
accept = dash media_type

-- * 5.3.3.  Accept-Charset

{-
Accept-Charset = 1#( ( charset / "*" ) [ weight ] )
-}


accept_charset :: Parser [(ByteString, Maybe Scientific)]
accept_charset = dash1 $ (,) <$> (charset <|> "*") <*> option Nothing (Just <$> weight)

-- * 5.3.4.  Accept-Encoding

{-
Accept-Encoding  = #( codings [ weight ] )
codings          = content-coding / "identity" / "*"
-}

accept_encoding :: Parser [(ByteString, Maybe Scientific)]
accept_encoding = dash ((,) <$> codings <*> option Nothing (Just <$> weight))

codings :: Parser ByteString
codings = content_coding <|> "identity" <|> "*"

-- * 5.3.5.  Accept-Language

{-
Accept-Language = 1#( language-range [ weight ] )
-}

accept_language :: Parser [(ByteString, Maybe Scientific)]
accept_language = dash1 ((,) <$> R4647.language_range <*> option Nothing (Just <$> weight))

-- * 5.4.  Authentication Credentials
{-
TODO:

+---------------------+--------------------------+
| Header Field Name   | Defined in...            |
+---------------------+--------------------------+
| Authorization       | Section 4.2 of [RFC7235] |
| Proxy-Authorization | Section 4.4 of [RFC7235] |
+---------------------+--------------------------+
-}

-- * 5.5.  Request Context
{-
The following request header fields provide additional information
about the request context, including information about the user, user
agent, and resource behind the request.
+-------------------+---------------+
| Header Field Name | Defined in... |
+-------------------+---------------+
| From              | Section 5.5.1 |
| Referer           | Section 5.5.2 |
| User-Agent        | Section 5.5.3 |
+-------------------+---------------+
-}

-- * 5.5.1.  From
from :: Parser R5322.NameAddress
from = R5322.mailbox

-- * 5.5.2.  Referer
-- TODO: add partial-URI
referer = R3986.absoluteUri

-- * 5.5.3.  User-Agent

-- |
-- >>> parse user_agent "CERN-LineMode/2.15 libwww/2.17b3\n"
-- Done "\n" ["CERN-LineMode/2.15","libwww/2.17b3"]
-- >>> parse user_agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/602.1.29 (KHTML, like Gecko) Version/9.1.1 Safari/601.6.17\n"
-- Done "\n" ["Mozilla/5.0","(Macintosh; Intel Mac OS X 10_11_4)","AppleWebKit/602.1.29","(KHTML, like Gecko)","Version/9.1.1","Safari/601.6.17"]
user_agent :: Parser [ByteString]
user_agent = (:) <$> product
                 <*> (many (rws *> (product <|> comment)))

product :: Parser ByteString
product = (\a b -> a <> "/" <> b) <$> token
                                  <*> option mempty (AC.char '/' *> product_version)

product_version :: Parser ByteString
product_version = token

-- * 7.  Response Header Fields
-- * 7.1.  Control Data
{-
   +-------------------+--------------------------+
   | Header Field Name | Defined in...            |
   +-------------------+--------------------------+
   | Age               | Section 5.1 of [RFC7234] |
   | Cache-Control     | Section 5.2 of [RFC7234] |
   | Expires           | Section 5.3 of [RFC7234] |
   | Date              | Section 7.1.1.2          |
   | Location          | Section 7.1.2            |
   | Retry-After       | Section 7.1.3            |
   | Vary              | Section 7.1.4            |
   | Warning           | Section 5.5 of [RFC7234] |
   +-------------------+--------------------------+

-}
-- * 7.1.1.  Origination Date
-- * 7.1.1.1.  Date/Time Formats

-- TODO: implement obs_date

-- |
-- >>> parse http_date "Fri, 31 Dec 1999 23:59:59 GMT\n"
-- Done "\n" 1999-12-31 23:59:59 UTC
http_date = imf_fixdate -- <|> obs_date

imf_fixdate :: Parser UTCTime
imf_fixdate
  = do day_name >> AC.char ',' >> sp
       ((d,m,y), tod) <- (,) <$> date1 <* sp
                             <*> time_of_day <* sp <* AC.string "GMT"
       let dt' = fromGregorianValid (toInteger y) m d
       case dt' of
         Just dt -> return . zonedTimeToUTC $ ZonedTime (LocalTime dt tod) utc
         Nothing -> fail "invalid date"

day_name :: Parser Int
day_name
  =   AC.string "Mon" *> pure 0
  <|> AC.string "Tue" *> pure 1
  <|> AC.string "Wed" *> pure 2
  <|> AC.string "Thu" *> pure 3
  <|> AC.string "Fri" *> pure 4
  <|> AC.string "Sat" *> pure 5
  <|> AC.string "Sun" *> pure 6
  <?> "dayName"

date1 :: Parser (Int, Int, Int)
date1 = (,,) <$> nDigitInt 2 (\x -> x >= 0 && x <= 31) <* sp
             <*> month <* sp
             <*> nDigitInt 4 (\x -> x >= 0 && x <= 2060)

month :: Parser Int
month
  =   AC.stringCI "Jan" *> pure 1
  <|> AC.stringCI "Feb" *> pure 2
  <|> AC.stringCI "Mar" *> pure 3
  <|> AC.stringCI "Apr" *> pure 4
  <|> AC.stringCI "May" *> pure 5
  <|> AC.stringCI "Jun" *> pure 6
  <|> AC.stringCI "Jul" *> pure 7
  <|> AC.stringCI "Aug" *> pure 8
  <|> AC.stringCI "Sep" *> pure 9
  <|> AC.stringCI "Oct" *> pure 10
  <|> AC.stringCI "Nov" *> pure 11
  <|> AC.stringCI "Dec" *> pure 12
  <?> "monthName"

time_of_day :: Parser TimeOfDay
time_of_day = do
  (h,m,s) <- (,,) <$> nDigitInt 2 zerotwentyfourP <* AC.char ':'
                  <*> nDigitInt 2 zerosixtyP      <* AC.char ':'
                  <*> nDigitInt 2 zerosixtyP
  return $ TimeOfDay h m (fromInteger $ toInteger s)
  where
    zerotwentyfourP x = x >= 0 && x <= 23
    zerosixtyP x = x >= 0 && x <= 59

-- * 7.1.1.2.  Date
date :: Parser UTCTime
date = http_date

-- * 7.1.2.  Location
location :: Parser URI
location = R3986.uriReference

-- * 7.1.3.  Retry-After

-- |
-- >>> parse retry_after "Fri, 31 Dec 1999 23:59:59 GMT\n"
-- Done "\n" (Left 1999-12-31 23:59:59 UTC)
-- >>> parse retry_after "3600\n"
-- Done "\n" (Right 3600)
retry_after :: Parser (Either UTCTime Int)
retry_after = eitherP http_date delay_seconds

delay_seconds :: Parser Int
delay_seconds = do
  sec <- AC.scientific
  case floatingOrInteger sec of
    Left  r -> fail "Not an integer"
    Right i -> return i

-- * 7.1.4.  Vary

data Star = Star deriving Show

-- |
-- >>> parseOnly vary "*"
-- Right (Left Star)
-- >>> parseOnly vary "accept-encoding, accept-language"
-- Right (Right ["accept-encoding","accept-language"])
vary :: Parser (Either Star [ByteString])
vary = eitherP (AC.char '*' *> pure Star) (dash1 field_name)

-- * 7.2.  Validator Header Fields
-- implemented in 7232

-- * 7.3.  Authentication Challenges
{- TODO: implement them
   +--------------------+--------------------------+
   | Header Field Name  | Defined in...            |
   +--------------------+--------------------------+
   | WWW-Authenticate   | Section 4.1 of [RFC7235] |
   | Proxy-Authenticate | Section 4.3 of [RFC7235] |
   +--------------------+--------------------------+
-}

-- * 7.4.  Response Context
-- Accept-Ranges in Rfc7233

-- * 7.4.1.  Allow
allow :: Parser [ByteString]
allow = dash method

-- * 7.4.2.  Server
server :: Parser [ByteString]
server = (:) <$> product <*> many (rws *> (product <|> comment))


