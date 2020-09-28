{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.Rfc5322
-- Copyright   :  Aycan iRiCAN 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Internet Message Format
-- <http://www.ietf.org/rfc/rfc5322.txt>

module Network.Parser.Rfc5322
  ( -- Header
    HeaderField
  , mkHeaderName
  , mkHeaderValue
  , mkHeaderField
  , headerName
  , headerValue
  , lookupHeader
  , contentTypeHeader
  , contentLengthHeader
  , contentDispositionHeader
  , contentTransferEncodingHeader
  , fromHeader
  , toHeader
  , subjectHeader
    
    -- Message
  , Message (..)
  , NameAddress (..)
  , StructuredHeader (..)
    
  -- parsers
  , mailbox
  , mailboxList
  , address
  , group
  , groupList
  , origDate
  , from
  , to
  , subject
  , message
  , references
  , replyTo
  , comments
  , keywords
  , addressList
  , resentTo
  , resentCc
  , resentBcc
  , resentSender
  , trace
  , traceReceived
  , traceReturn
  , receivedDate
  , returnPath
  , path
  , received
  , receivedToken
  , resentMsgId
  , resentFrom
  , resentDate
  , inReplyTo
  , messageId
  , bcc
  , cc
  , sender
  , specials
  , fields
  , qtext
  , qcontent
  , dateTime
  , optionalField
  )
  where

import Control.Applicative
import Control.Monad (join)
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString
  ( ByteString,
    cons,
    pack,
    singleton,
  )
import qualified Data.ByteString.Char8 as BSC
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.List (find)
import Data.Monoid
import Data.Time
import Data.Word (Word8)
import qualified Network.Parser.Rfc2822 as R2822
import Network.Parser.Rfc5234
import qualified Network.Parser.RfcCommon as RfcCommon
import Prelude hiding (id, take, takeWhile)

-- * Interface

-- Message Headers
type HeaderName  = CI ByteString
mkHeaderName :: CI ByteString -> HeaderName
mkHeaderName name = name

headerName :: HeaderField -> CI ByteString
headerName = fst

type HeaderValue = ByteString
mkHeaderValue :: ByteString -> HeaderValue
mkHeaderValue x = x

headerValue :: HeaderField -> ByteString
headerValue = snd

type HeaderField = (HeaderName, HeaderValue)
mkHeaderField :: CI ByteString -> ByteString -> HeaderField
mkHeaderField name value = (mkHeaderName name, mkHeaderValue value)

data Message
  = Message { messageFields :: [HeaderField]
            , messageBody   :: Maybe ByteString
            , messageParts  :: [Message] -- parts for mutlipart messages
            } deriving (Eq, Show)

-- * Common Accessors
lookupHeader :: HeaderName -> Message -> Maybe HeaderField
lookupHeader name msg = find ((== name) . headerName) (messageFields msg)

mkHeaderQuery :: HeaderName -> Message -> Maybe HeaderValue
mkHeaderQuery key msg = headerValue <$> lookupHeader key msg

-- Common Headers
contentTypeHeader, contentLengthHeader, contentDispositionHeader, contentTransferEncodingHeader,fromHeader, toHeader, subjectHeader :: Message -> Maybe HeaderValue
contentTypeHeader              = mkHeaderQuery "content-type"
contentLengthHeader            = mkHeaderQuery "content-length"
contentDispositionHeader       = mkHeaderQuery "content-disposition"
contentTransferEncodingHeader  = mkHeaderQuery "content-transfer-encoding"
fromHeader                     = mkHeaderQuery "from"
toHeader                       = mkHeaderQuery "to"
subjectHeader                  = mkHeaderQuery "subject"

-- Helper Data Structures
data StructuredHeader
  = TraceField      Trace
  | ResentDate      UTCTime
  | ResentFrom      [NameAddress]
  | ResentSender    NameAddress
  | ResentTo        [NameAddress]
  | ResentCc        [NameAddress]
  | ResentBcc       [NameAddress]
  | ResentMessageID ByteString
  | Date            UTCTime
  | From            [NameAddress]
  | Sender          NameAddress
  | ReplyTo         [NameAddress]
  | To              [NameAddress]
  | Cc              [NameAddress]
  | Bcc             [NameAddress]
  | MessageID       ByteString
  | InReplyTo       [ByteString]
  | References      [ByteString]
  | Subject         ByteString
  | Comments        ByteString
  | Keywords        [ByteString]
  | OptionalField   (CI ByteString) ByteString
  deriving (Eq, Show)

data Received
  = Received { receivedNameVals :: [ByteString]
             , receivedDate     :: UTCTime
             } deriving (Eq, Show)

data Trace
  = Trace { traceReturn   :: Maybe ByteString
          , traceReceived :: [Received]
          } deriving (Eq, Show)

data NameAddress
    = NameAddress
      { naName :: Maybe ByteString
      , naAddr :: ByteString
      } deriving (Eq, Show)

quotedPair :: Parser ByteString
quotedPair = (\a b -> pack [a,b]) <$> word8 92 <*> (vchar <|> wsp)

-- | * 3.2.2.  Folding White Space and Comments
-- Parse Folding Whitespace
fws :: Parser ByteString
fws = do
  _ <- option [] (many wsp *> RfcCommon.asList crlf)
  _ <- many1 wsp
  return " "

-- Parse ctext
ctext :: Parser Word8
ctext = satisfy (\w ->
                  w >= 33 && w <= 39
                  || (w >= 42 && w <= 91)
                  || (w >= 93 && w <= 126))

ccontent :: Parser ByteString
ccontent = (singleton <$> ctext) <|> quotedPair <|> comment

-- Parse a comment
comment :: Parser ByteString
comment = do
  _ <- word8 40
  r1 <- many (option mempty fws *> ccontent)
  r2 <- option mempty fws
  _ <- word8 41
  return $ "(" <> BSC.concat r1 <> r2 <> ")"

cfws :: Parser ByteString
cfws = BSC.concat <$> (many1 (option mempty fws *> comment) <* option mempty fws
                      <|> RfcCommon.asList fws)

-- | * 3.2.3. Atom
atext :: Parser Word8
atext = alpha <|> digit <|> satisfy p
  where p = inClass "!#$%&'*+/=?^_`{|}~-"

atom :: Parser ByteString
atom = ocfws *> (pack <$> many1 atext) <* ocfws

dotAtomText :: Parser ByteString
dotAtomText = do
  t1 <- many1 atext
  t2 <- many ((:) <$> word8 46 <*> many1 atext)
  return (pack t1 <> pack (join t2))

dotAtom :: Parser ByteString
dotAtom = ocfws *> dotAtomText <* ocfws

specials :: Parser ByteString
specials = singleton <$> (satisfy p <|> dquote)
  where p = inClass "()<>[]:;@\\,."

-- | * 3.2.4.  Quoted Strings

qtext :: Parser Word8
qtext = satisfy p
  where
    p w = (w == 33)
          || (w >= 35 && w <= 91)
          || (w >= 93 && w <= 126)
{-# INLINABLE qtext #-}

qcontent :: Parser ByteString
qcontent = pack <$> (RfcCommon.asList qtext <|> RfcCommon.quotedPair)

-- | * 3.2.5.  Miscellaneous Tokens
word :: Parser ByteString
word = atom <|> R2822.quotedString

phrase :: Parser [ByteString]
phrase = many1 word

-- >>> parse unstructured "asda sdasd asd asd\n    asdasd asd\n\n"
-- Done "\n\n" "asda sdasd asd asd asdasd asd"

-- >>> parseOnly unstructured " Thu, 12 Dec 2019 09:54:09 +0000\n"
-- Right " Thu, 12 Dec 2019 09:54:09 +0000"

-- >>> parseOnly unstructured " \n Thu, 12 Dec 2019 09:54:09 +0000\n"
-- Right " Thu, 12 Dec 2019 09:54:09 +0000"
unstructured :: Parser ByteString
unstructured = do
  r1 <- many ((<>) <$> option mempty fws <*> (singleton <$> vchar))
          <* many wsp
  return $ BSC.dropWhile (== ' ') $ BSC.concat r1

{- * 3.3 Date and Time Specification

 date-time       =       [ day-of-week "," ] date FWS time [CFWS]
 day-of-week     =       ([FWS] day-name) / obs-day-of-week
 day-name        =       "Mon" / "Tue" / "Wed" / "Thu" /
                         "Fri" / "Sat" / "Sun"
 date            =       day month year
 year            =       4*DIGIT / obs-year
 month           =       (FWS month-name FWS) / obs-month
 month-name      =       "Jan" / "Feb" / "Mar" / "Apr" /
                         "May" / "Jun" / "Jul" / "Aug" /
                         "Sep" / "Oct" / "Nov" / "Dec"
 day             =       ([FWS] 1*2DIGIT) / obs-day
 time            =       time-of-day FWS zone
 time-of-day     =       hour ":" minute [ ":" second ]
 hour            =       2DIGIT / obs-hour
 minute          =       2DIGIT / obs-minute
 second          =       2DIGIT / obs-second
 zone            =       (( "+" / "-" ) 4DIGIT) / obs-zone

-}
twoDigitInt :: (Int -> Bool) -> Parser Int
twoDigitInt validator = do
  (a,b) <- (,) <$> AC.digit <*> AC.digit
  let res = digitToInt a * 10 + digitToInt b
  if validator res then return res else
    fail "Invalid two digit integer"

-- TODO: add obsoletes
-- >>> parse dateTime "Wed, 20 Apr 2016 04:59:01 -0700 (PDT)\n\n"
-- Done "\n\n" 2016-04-20 11:59:01 UTC
-- >>> parse dateTime "Mon, 21 Sep 1980 10:01:02 +0230\n\n"
-- Done "\n\n" 1980-09-21 07:31:02 UTC
dateTime :: Parser UTCTime
dateTime = do
  _ <- option 0 (Network.Parser.Rfc5322.dayOfWeek <* AC.char ',')
  ((d,m,y), (tod,tz)) <- (,) <$> date <*> time <* ocfws
  let dt' = fromGregorianValid (toInteger y) m d
  case dt' of
    Just dt -> return . zonedTimeToUTC $ ZonedTime (LocalTime dt tod) tz
    Nothing -> fail "invalid day"

dayOfWeek :: Parser Int
dayOfWeek = option mempty fws *> dayName

dayName :: Parser Int
dayName
  =   AC.stringCI "Mon" $> 0
  <|> AC.stringCI "Tue" $> 1
  <|> AC.stringCI "Wed" $> 2
  <|> AC.stringCI "Thu" $> 3
  <|> AC.stringCI "Fri" $> 4
  <|> AC.stringCI "Sat" $> 5
  <|> AC.stringCI "Sun" $> 6
  <?> "dayName"

-- >>> parseOnly date "21 Sep 1980"
-- Right (21,9,1980)
date :: Parser (Int, Int, Int)
date = (,,) <$> day <*> month <*> year

-- >>> parseOnly day "21"
-- Right 21
-- >>> parseOnly day "41"
-- Left "Failed reading: Invalid two digit integer"
day :: Parser Int
day = option mempty fws *> twoDigitInt (\x -> 0<=x && x <= 31) <* fws

month :: Parser Int
month
  =   AC.stringCI "Jan" $> 1
  <|> AC.stringCI "Feb" $> 2
  <|> AC.stringCI "Mar" $> 3
  <|> AC.stringCI "Apr" $> 4
  <|> AC.stringCI "May" $> 5
  <|> AC.stringCI "Jun" $> 6
  <|> AC.stringCI "Jul" $> 7
  <|> AC.stringCI "Aug" $> 8
  <|> AC.stringCI "Sep" $> 9
  <|> AC.stringCI "Oct" $> 10
  <|> AC.stringCI "Nov" $> 11
  <|> AC.stringCI "Dec" $> 12
  <?> "monthName"

-- >>> parseOnly year "2015"
-- Right 2015
year :: Parser Int
year =  conv <$> (fws *> count 4 AC.digit <* fws)
  where conv = sum . zipWith (*) [1000, 100, 10, 1] . fmap digitToInt

-- >>> parseOnly time "04:20:05 +0200"
-- Right (04:20:05,+0200)
time :: Parser (TimeOfDay, TimeZone)
time = (,) <$> timeOfDay <*> zone

-- >>> parseOnly timeOfDay "04:20:01"
-- Right 04:20:01
timeOfDay :: Parser TimeOfDay
timeOfDay = do
  (h,m) <- (,) <$> hour <* AC.char ':' <*> minute
  s <- option 0 (AC.char ':' *> second)
  return $ TimeOfDay h m (fromInteger $ toInteger s)

hour, minute, second :: Parser Int
hour   = twoDigitInt (\x -> (0 <= x) && x <= 23)
minute = twoDigitInt (\x -> (0 <= x) && x <= 59)
second = twoDigitInt (\x -> (0 <= x) && x <= 59)

-- >>> parseOnly zone "-0200"
-- Right -0200
-- >>> parseOnly zone "+0200"
-- Right +0200
zone :: Parser TimeZone
zone = do
  sign <- fws *> eitherP (AC.char '-') (AC.char '+')
  case sign of
    Left  _ -> minutesToTimeZone . negate . minutes <$> fourDigit
    Right _ -> minutesToTimeZone . minutes <$> fourDigit
  where
    minutes (a,b,c,d) = (((a*10)+b) * 60) + ((c*10) + d)
    fourDigit :: Parser (Int, Int, Int, Int)
    fourDigit = (\[a,b,c,d] -> (a,b,c,d)) . map digitToInt <$> count 4 AC.digit

-- | * 3.4. Address Specification
address :: Parser [NameAddress]
address = RfcCommon.asList mailbox <|> group

mailbox :: Parser NameAddress
mailbox = nameAddr <|> (NameAddress Nothing <$> addrSpec)

nameAddr :: Parser NameAddress
nameAddr = NameAddress <$> optional displayName
                        <*> angleAddr

angleAddr :: Parser ByteString
angleAddr = ocfws *> word8 60 *> addrSpec <* word8 62 <* ocfws

group :: Parser [NameAddress]
group = displayName *> word8 58 *> option [] groupList <* word8 59 <* ocfws

displayName :: Parser ByteString
displayName = BSC.unwords <$> phrase

mailboxList :: Parser [NameAddress]
mailboxList = (:) <$> mailbox <*> many (AC.char ',' *> mailbox)

addressList :: Parser [NameAddress]
addressList = (++) <$> address <*> (join <$> many (AC.char ',' *> address))

groupList :: Parser [NameAddress]
groupList = mailboxList <|> (cfws $> [])

-- | 3.4.1.  Addr-Spec Specification
addrSpec :: Parser ByteString
addrSpec = ret <$> localPart <* AC.char '@' <*> domain
  where ret l r = l <> "@" <> r

localPart :: Parser ByteString
localPart = dotAtom <|> R2822.quotedString

domain :: Parser ByteString
domain = dotAtom <|> domainLiteral

domainLiteral :: Parser ByteString
domainLiteral = do
  _ <- ocfws >> word8 91
  res <- BSC.concat <$> many ((<>) <$> option mempty fws *> dcontent)
  _ <- word8 92 >> ocfws
  return ("[" <> res <> "]")

dcontent :: Parser ByteString
dcontent = (pack <$> many1 dtext) <|> quotedPair

dtext :: Parser Word8
dtext = satisfy p
  where p w = (w>=33 && w<=90) || (w>=94 && w<= 126)

-- 3.5.  Overall Message Syntax
message :: Parser Message
message = Message <$> fields <* crlf <*> optional body <*> pure []

-- We allow headers without any value in this implementation.

-- >>> :set -XOverloadedStrings
-- >>> parseOnly fields "Date: foo\n\n"
-- Right [("Date","foo")]
-- >>> parseOnly fields "Message-ID:\n <verylongstringthtat.has.dots.and@an.at.sign.com>\n"
-- Right [("Message-ID","<verylongstringthtat.has.dots.and@an.at.sign.com>")]
-- >>> parseOnly fields "x-ms-publictraffictype: Email\n"
-- Right [("x-ms-publictraffictype","Email")]
-- >>> parseOnly fields "X-MS-TNEF-Correlator:\nx-ms-publictraffictype: Email\n"
-- Right [("X-MS-TNEF-Correlator",""),("x-ms-publictraffictype","Email")]
-- >>> parseOnly fields "X-MS-TNEF-Correlator:\nContent-Type: text/rfc822\n"
-- Right [("X-MS-TNEF-Correlator",""),("Content-Type","text/rfc822")]
-- >>> parseOnly fields "Content-Type: multipart/alternative; boundary=\"MCBoundary=_11912230343231881\"\n"
-- Right [("Content-Type","multipart/alternative; boundary=\"MCBoundary=_11912230343231881\"")]

fields :: Parser [HeaderField]
fields = many headerField
  where
    headerField :: Parser HeaderField
    headerField = do
      key <- CI.mk <$> fieldName <* word8 58
      val <- unstructured
      _ <- crlf
      return $ mkHeaderField key val
      
body :: Parser ByteString
body = fst <$> match (text998 `sepBy` crlf)

text998 :: Parser ByteString
text998 = textMax 998

-- | TODO: optimize it...
textMax :: Int -> Parser ByteString
textMax 0 = return mempty
textMax n = (cons <$> text <*> textMax (n-1)) <|> return mempty

-- Characters excluding CR and LF
text :: Parser Word8
text = satisfy p
  where
    p w = (w >= 1 && w <= 9)
          || w == 11
          || w == 12
          || (w >= 14 && w <= 127)

-- | 3.6.1. The origination date field
origDate :: Parser UTCTime
origDate = dateTime

-- | 3.6.2. Originator fields
from :: Parser [NameAddress]
from = mailboxList

sender :: Parser NameAddress
sender = mailbox

replyTo :: Parser [NameAddress]
replyTo = addressList

-- | 3.6.3. Destination address fields
to, cc, bcc :: Parser [NameAddress]
to  = addressList
cc  = addressList
bcc = option [] (addressList <|> (cfws $> []))

-- | 3.6.4. Identification fields
messageId :: Parser ByteString
messageId = msgId

inReplyTo :: Parser [ByteString]
inReplyTo = many1 msgId

references :: Parser [ByteString]
references = many1 msgId

msgId :: Parser ByteString
msgId = ocfws *> word8 60 *> mid <* word8 62 <* ocfws
  where
    mid = res <$> idLeft <* word8 64 <*> idRight
    res l r = l <> "@" <> r

idLeft :: Parser ByteString
idLeft = dotAtomText

idRight :: Parser ByteString
idRight = dotAtomText <|> noFoldLiteral

noFoldLiteral :: Parser ByteString
noFoldLiteral = fst <$> match ("[" >> many dtext >> "]")

-- | 3.6.5. Informational fields
subject :: Parser ByteString
subject  = unstructured

comments :: Parser ByteString
comments = unstructured

keywords :: Parser [ByteString]
keywords = kwrds
  where
    kwrds :: Parser [ByteString]
    kwrds = do
      x <- phrase
      xs <- many (AC.char ',' *> phrase)
      return $ x ++ join xs

-- | 3.6.6. Resent fields
resentDate :: Parser UTCTime
resentDate = dateTime

resentFrom, resentTo, resentCc, resentBcc :: Parser [NameAddress]
resentFrom   = mailboxList
resentTo     = addressList
resentCc     = addressList
resentBcc    = option [] (addressList <|> (cfws $> []))

resentMsgId :: Parser ByteString
resentMsgId  = msgId

resentSender :: Parser NameAddress
resentSender = mailbox

-- | 3.6.7. Trace fields

trace :: Parser Trace
trace = Trace <$> optional returnPath
              <*> many1 received

returnPath :: Parser ByteString
returnPath = path

path :: Parser ByteString
path = angleAddr <|> (ocfws >> AC.char '<' >> ocfws >> AC.char '>' >> ocfws >> pure "<>")

received :: Parser Received
received = Received <$> (many receivedToken <* AC.char ';') <*> (dateTime <?> "date-time")

receivedToken :: Parser ByteString
receivedToken = word <|> angleAddr <|> addrSpec <|> domain

-- | 3.6.8. Optional fields
optionalField :: Parser ByteString
optionalField = unstructured <* crlf

fieldName :: Parser ByteString
fieldName = pack <$> many1 ftext

ftext :: Parser Word8
ftext = satisfy $ \w -> (w >= 33 && w <= 57) || (w >= 59 && w <= 126)

-- | Utils
ocfws :: Parser ByteString
ocfws = option mempty cfws

