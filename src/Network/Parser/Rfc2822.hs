{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.Rfc2822
-- Copyright   :  Aycan iRiCAN 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Internet Message Format
-- <http://www.ietf.org/rfc/rfc2822.txt>

module Network.Parser.Rfc2822 where

import Control.Applicative
  ( Alternative (many, (<|>)),
    optional,
  )
import Control.Monad (join)
import Data.Attoparsec.ByteString
  ( Parser,
    choice,
    count,
    eitherP,
    inClass,
    many',
    many1,
    option,
    satisfy,
    sepBy,
    try,
    word8,
    (<?>),
  )
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString as B
  ( ByteString,
    concat,
    intercalate,
    pack,
  )
import qualified Data.ByteString.Char8 as BSC
import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.List as L (concat, intercalate)
import Data.Maybe (catMaybes)
import Data.Time
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    TimeZone,
    UTCTime,
    ZonedTime (ZonedTime),
    fromGregorianValid,
    minutesToTimeZone,
    zonedTimeToUTC,
  )
import Data.Word (Word8)
import Network.Parser.Rfc2234
  ( alpha,
    charPred,
    crlf,
    ctlPred,
    digit,
    dquote,
    spPred,
    wsp,
  )
import Network.Parser.RfcCommon (asList, qdtext, quotedPair)
import Prelude hiding (id, take, takeWhile)

--  ** General Utils

-- | Used to generate arbitrary header parsers
header :: ByteString -> Parser a -> Parser a
header key p = AC.stringCI (key <> ": ") *> p <* crlf

-- ** 3.2.1. Primitive Tokens
noWsCtlPred :: Word8 -> Bool
noWsCtlPred w
  = (w >= 1 && w <= 8)
    || (w == 11) || (w == 12)
    || (w <= 14 && w >= 31)
    || w == 127

noWsCtl :: Parser Word8
noWsCtl = satisfy noWsCtlPred

-- | Parse a text element and return corresponding Word8
text :: Parser Word8
text = satisfy $ \w ->
       (w >= 1 && w<=9)
       || w == 11
       || w == 12
       || (w >= 14 && w<=127)

-- Prelude.map Data.Char.ord "()<>[]:;@\\,.\""

-- specialsSet ::[Word8]
-- specialsSet = [40,41,60,62,91,93,58,59,64,92,44,46,34]
specialsPred :: Word8 -> Bool
specialsPred = inClass "()<>[]:;@\\,.\"" -- F.memberWord8 w (F.fromList specialsSet)

-- | Parse a special
specials :: Parser Word8
specials = satisfy specialsPred

-- ** 3.2.3. Folding white space and comments

-- Parse Whitespaces
wsps :: Parser [Word8]
wsps = many1 wsp

-- Parse Folding Whitespace
fws :: Parser [Word8]
fws = [32] <$ many1 (choice [wsps, crlf *> wsps])

-- Parse ctext
ctext :: Parser Word8
ctext = noWsCtl <|> satisfy rest
    where
      rest w = (w >= 33 && w <= 39)
               || (w >= 42 && w <= 91)
               || (w >= 93 && w <= 126)

-- Parse a comment
comment :: Parser ByteString
comment = do
  _ <- word8 40
  r1 <- many (option [] fws *> ccontent)
  r2 <- pack <$> option [] fws
  _ <- word8 41
  return $ "(" <> B.concat r1 <> r2 <> ")"
  where
    ccontent :: Parser ByteString
    ccontent = choice [pack <$> many1 ctext, pack <$> quotedPair, comment]

cfws :: Parser ByteString
cfws = B.concat <$> many1 (choice [pack <$> fws, comment])

ocfws :: Parser ByteString
ocfws = option mempty cfws

-- | * 3.2.4. Atom
atextPred :: Word8 -> Bool
atextPred w = charPred w && not (ctlPred w || spPred w || specialsPred w)

atext :: Parser Word8
atext = satisfy atextPred

atom :: Parser ByteString
atom = pack <$> (ocfws *> many1 atext <* ocfws)

dotAtomText :: Parser ByteString
dotAtomText = pack . L.intercalate [46] <$> sepBy (many1 atext) (word8 46)

dotAtom :: Parser ByteString
dotAtom = ocfws *> dotAtomText <* ocfws

-- | * 3.2.5. Quoted strings
qtextPred :: Word8 -> Bool
qtextPred w = noWsCtlPred w
               || w == 33
               || (w >= 35 && w <= 91)
               || (w >= 93 && w <= 126)

qtext :: Parser Word8
qtext = satisfy qtextPred
{-# INLINABLE qtext #-}


-- parseOnly qcontent "asd"
-- Just "a"
-- parseOnly qcontent "\afoo"
-- Just "\a"
qcontent :: Parser ByteString
qcontent = pack <$> (asList qtext <|> quotedPair)

-- | Parse a quoted string

-- >>> let d = "\r\n\t\a\b\"\\"
-- >>> map Data.Char.ord d
-- [13,10,9,7,8,34,92]

-- >>> :set -XOverloadedStrings
-- >>> parseOnly quotedString "\"foo is on the bar\""
-- Right "foo is on the bar"

quotedString :: Parser ByteString
quotedString = pack . join <$> (dquote *> many (asList qdtext <|> quotedPair) <* dquote)

-- | * 3.2.6. Miscellaneous tokens
word :: Parser ByteString
word = atom <|> quotedString

phrase :: Parser ByteString
phrase = B.intercalate " " <$> many1 word

utext :: Parser Word8
utext = noWsCtl <|> satisfy (\w -> w>=33 && w<=126)

-- >>> parseOnly unstructured "  a b c \n"
-- Right " a b c "
unstructured :: Parser ByteString
unstructured = pack <$> do
  r1 <- option [] fws
  r2 <- many ((:) <$> utext <*> option [] fws)
  return (r1 ++ join r2)

{- | * 3.3 Date and Time Specification

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
  (a, b) <- (,) <$> AC.digit <*> AC.digit
  let res = digitToInt a * 10 + digitToInt b
  if validator res
    then return res
    else fail "Invalid two digit integer"

-- TODO: add obsoletes
-- >>> parse dateTime "Wed, 20 Apr 2016 04:59:01 -0700 (PDT)\n\n"
-- Done "\n\n" 2016-04-20 11:59:01 UTC
-- >>> parse dateTime "Mon, 21 Sep 1980 10:01:02 +0230\n\n"
-- Done "\n\n" 1980-09-21 07:31:02 UTC
dateTime :: Parser UTCTime
dateTime = do
  _ <- option 0 (Network.Parser.Rfc2822.dayOfWeek <* AC.char ',')
  ((d, m, y), (tod, tz)) <- (,) <$> (date <* fws) <*> (time <* ocfws)
  let dt' = fromGregorianValid (toInteger y) m d
  case dt' of
    Just dt -> return . zonedTimeToUTC $ ZonedTime (LocalTime dt tod) tz
    Nothing -> fail "invalid day"

dayOfWeek :: Parser Int
dayOfWeek = option [] fws *> dayName

dayName :: Parser Int
dayName = AC.stringCI "Mon" $> 0
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

-- >>> parseOnly year "2015"
-- Right 2015
year :: Parser Int
year = sum . zipWith (*) [1000, 100, 10, 1] . fmap digitToInt <$> count 4 AC.digit

-- >>> parseOnly month " Feb"
-- Right 2
month :: Parser Int
month = option [] fws *> monthName <* option [] fws

monthName :: Parser Int
monthName = AC.stringCI "Jan" $> 1
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

-- >>> parseOnly day "21"
-- Right 21
-- >>> parseOnly day "41"
-- Left "Failed reading: Invalid two digit integer"
day :: Parser Int
day = option [] fws *> twoDigitInt (\x -> 0<=x && x <= 31)

-- >>> parseOnly time "04:20:05 +0200"
-- Right (04:20:05,+0200)
time :: Parser (TimeOfDay, TimeZone)
time = (,) <$> timeOfDay <* fws <*> zone

-- >>> parseOnly timeOfDay "04:20:01"
-- Right 04:20:01
timeOfDay :: Parser TimeOfDay
timeOfDay = do
  (h,m) <- (,) <$> hour <* AC.char ':' <*> minute
  s <- option 0 (AC.char ':' *> second)
  return $ TimeOfDay h m (fromInteger $ toInteger s)

hour, minute, second :: Parser Int
hour   = twoDigitInt (\x -> (0 <= x) && x <= 60)
minute = twoDigitInt (\x -> (0 <= x) && x <= 60)
second = twoDigitInt (\x -> (0 <= x) && x <= 60)

-- >>> parseOnly zone "-0200"
-- Right -0200
-- >>> parseOnly zone "+0200"
-- Right +0200
zone :: Parser TimeZone
zone = do
  sign <- eitherP (AC.char '-') (AC.char '+')
  case sign of
    Left  _ -> minutesToTimeZone . negate . minutes <$> fourDigit
    Right _ -> minutesToTimeZone . minutes <$> fourDigit
  where
    minutes (a,b,c,d) = (((a*10)+b) * 60) + ((c*10) + d)
    fourDigit :: Parser (Int, Int, Int, Int)
    fourDigit = (\[a,b,c,d] -> (a,b,c,d)) . map digitToInt <$> count 4 AC.digit

-- | * 3.4. Address Specification
address :: Parser [NameAddress]
address = try (asList mailbox)
          <|> group

mailbox :: Parser NameAddress
mailbox = try nameAddr
      <|> NameAddress Nothing <$> addrSpec

nameAddr :: Parser NameAddress
nameAddr = do
  n <- option mempty displayName
  a <- angleAddr
  return $
    if "" == n
      then NameAddress Nothing a
      else NameAddress (Just n) a

angleAddr :: Parser ByteString
angleAddr =
  ocfws *> word8 60 *> addrSpec <* word8 62 <* ocfws

group :: Parser [NameAddress]
group =
  displayName *> word8 58 *> option [] mailboxList
    <* ocfws
    <* word8 59
    <* ocfws

displayName :: Parser ByteString
displayName = phrase

mailboxList :: Parser [NameAddress]
mailboxList = sepBy mailbox (word8 44)

addressList :: Parser [NameAddress]
addressList = Prelude.concat <$> sepBy address (word8 44)

addrSpec :: Parser ByteString
addrSpec = ret <$> localPart <* AC.char '@' <*> domain
  where
    ret l r = l <> "@" <> r

localPart :: Parser ByteString
localPart = dotAtom <|> quotedString

domain :: Parser ByteString
domain = dotAtom <|> (pack <$> domainLiteral)

domainLiteral :: Parser [Word8]
domainLiteral =
  ocfws *> word8 91
    *> ((\xs -> [91] ++ L.concat xs ++ [92]) <$> many' (option [] fws *> dcontent))
    <* word8 92

dcontent :: Parser [Word8]
dcontent = many1 dtext <|> quotedPair

dtextPred :: Word8 -> Bool
dtextPred w = noWsCtlPred w || (w >= 33 && w <= 90) || (w >= 94 && w <= 126)

dtext :: Parser Word8
dtext = satisfy dtextPred

-- | * 3.5 Overall Message Syntax
data Message
  = Message { messageFields :: [Field]
            , messageBody   :: ByteString
            } deriving (Eq, Show)

data Field = TraceField Trace
           | ResentDate UTCTime
           | ResentFrom [NameAddress]
           | ResentSender NameAddress
           | ResentTo [NameAddress]
           | ResentCc [NameAddress]
           | ResentBcc [NameAddress]
           | ResentMessageID ByteString
           | Date UTCTime
           | From [NameAddress]
           | Sender NameAddress
           | ReplyTo [NameAddress]
           | To [NameAddress]
           | Cc [NameAddress]
           | Bcc [NameAddress]
           | MessageID ByteString
           | InReplyTo [ByteString]
           | References [ByteString]
           | Subject ByteString
           | Comments ByteString
           | Keywords [ByteString]
           | OptionalField ByteString ByteString
             deriving (Eq, Show)

message :: Parser Message
message = Message <$> fields <*> (option 0 crlf *> body)

fields :: Parser [Field]
fields
  = many (   TraceField <$> trace
         <|> resentDate
         <|> resentFrom
         <|> resentSender
         <|> resentTo
         <|> resentCc
         <|> resentBcc
         <|> resentMsgId
         <|> origDate
         <|> from
         <|> sender
         <|> replyTo
         <|> to
         <|> cc
         <|> bcc
         <|> messageId
         <|> inReplyTo
         <|> references
         <|> subject
         <|> comments
         <|> keywords
         <|> optionalField
         )

body :: Parser ByteString
body = BSC.pack <$> many AC.anyChar

--  ** 3.6 Field Definitions
-- | 3.6.1. The origination date field
origDate :: Parser Field
origDate = header "Date" (Date <$> dateTime)

-- | 3.6.2. Originator fields
from :: Parser Field
from = header "From" (From <$> mailboxList)

sender :: Parser Field
sender = header "Sender" (Sender <$> mailbox)

replyTo :: Parser Field
replyTo = header "Reply To" (ReplyTo <$> mailboxList)

-- | 3.6.3. Destination address fields
to :: Parser Field
to = header "To"  (To <$> addressList)

cc :: Parser Field
cc = header "Cc"  (Cc <$> addressList)

bcc :: Parser Field
bcc = header "Bcc" (Bcc <$> (addressList <|> (many cfws $> [])))

-- | 3.6.4. Identification fields
messageId :: Parser Field
messageId  = header "Message-ID"  (MessageID <$> msgId)

inReplyTo :: Parser Field
inReplyTo = header "In-Reply-To" (InReplyTo <$> many1 msgId)

references :: Parser Field
references  = header "References"  (References <$> many1 msgId)

msgId :: Parser ByteString
msgId = ocfws *> word8 60 *> mid <* word8 62 <* ocfws
  where
    mid = res <$> idLeft <* word8 64 <*> idRight
    res l r = l <> "@" <> r

idLeft :: Parser ByteString
idLeft = dotAtomText <|> pack <$> noFoldQuote

idRight :: Parser ByteString
idRight = dotAtomText <|> pack <$> noFoldLiteral

noFoldQuote :: Parser [Word8]
noFoldQuote = do
  l <- dquote
  m <- L.concat <$> many' (option [] (asList qtext) <|> quotedPair)
  r <- dquote
  return $ [l] ++ m ++ [r]

noFoldLiteral :: Parser [Word8]
noFoldLiteral = do
  l <- word8 91 -- '['
  m <- L.concat <$> many' (option [] (asList dtext) <|> quotedPair)
  r <- word8 93 -- ']'
  return $ [l] ++ m ++ [r]

-- | 3.6.5. Informational fields
subject :: Parser Field
subject  = header "Subject" (Subject <$> unstructured)

comments :: Parser Field
comments = header "Comments" (Comments <$> unstructured)

keywords :: Parser Field
keywords = header "Keywords" (Keywords <$> kwrds)
  where
    kwrds :: Parser [ByteString]
    kwrds = do
      x <- phrase
      xs <- many (AC.char ',' *> phrase)
      return $ x:xs

-- | 3.6.6. Resent fields
resentDate, resentFrom, resentSender, resentTo,resentCc,resentBcc,resentMsgId :: Parser Field
resentDate   = header "Resent-Date"       (ResentDate      <$> dateTime)
resentFrom   = header "Resent-From"       (ResentFrom      <$> mailboxList)
resentSender = header "Resent-Sender"     (ResentSender    <$> mailbox)
resentTo     = header "Resent-To"         (ResentTo        <$> addressList)
resentCc     = header "Resent-Cc"         (ResentCc        <$> addressList)
resentBcc    = header "Resent-Bcc"        (ResentBcc       <$> (addressList <|> (many cfws $> [])))
resentMsgId  = header "Resent-Message-ID" (ResentMessageID <$> msgId)

-- | 3.6.7. Trace fields

trace :: Parser Trace
trace = Trace <$> optional returnPath
              <*> many1 received

returnPath :: Parser ByteString
returnPath = header "Return-Path" path

path :: Parser ByteString
path = angleAddr <|> (ocfws >> AC.char '<' >> ocfws >> AC.char '>' >> ocfws >> pure "<>")

received :: Parser Received
received = header "Received" (Received <$> (nameValList <* AC.char ';') <*> (dateTime <?> "date-time"))

-- >>> parseOnly nameValList "by 10.112.112.39 with SMTP id in7csp2918309lbb"
-- Right [("by","10.112.112.39"),("with","SMTP"),("id","in7csp2918309lbb")]
nameValList :: Parser [(ByteString, ByteString)]
nameValList = do
  _ <- ocfws
  n  <- optional nameValPair
  ns <- many (ocfws *> (Just <$> nameValPair))
  return . catMaybes $ n:ns

nameValPair :: Parser (ByteString, ByteString)
nameValPair = (,) <$> (itemName <* cfws) <*> itemValue

itemName :: Parser ByteString
itemName = do
  x  <- alpha
  xs <- many (alpha <|> digit <|> word8 45)
  return . pack $ x : xs

-- >>> parseOnly itemValue "10.112.112.39"
-- Right "10.112.112.39"
itemValue :: Parser ByteString
itemValue = (B.concat <$> many1 angleAddr)
        <|> (addrSpec <?> "addr-spec")
        <|> (domain   <?> "domain")
        <|> (atom     <?> "atom")
        <|> (msgId    <?> "msg-id")


-- | 3.6.8. Optional fields
-- TODO start here..
optionalField :: Parser Field
optionalField = OptionalField <$> (fieldName <* word8 58) <*> (unstructured <* crlf)

fieldName :: Parser ByteString
fieldName = pack <$> many1 ftext

ftext :: Parser Word8
ftext = satisfy $ \w -> (w >= 33 && w <= 57) || (w >= 59 && w <= 126)

-- * ADTs
data Received = Received 
  { receivedNameVals :: [(ByteString, ByteString)]
  , receivedDate     :: UTCTime
  } deriving (Eq, Show)

data Trace = Trace 
  { traceReturn   :: Maybe ByteString
  , traceReceived :: [Received]
  } deriving (Eq, Show)
                     
data NameAddress = NameAddress
  { naName :: Maybe ByteString
  , naAddr :: ByteString
  } deriving (Eq, Show)
