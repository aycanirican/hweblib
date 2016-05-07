{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.Rfc5322
-- Copyright   :  Aycan iRiCAN 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Internet Message Format
-- <http://www.ietf.org/rfc/rfc5322.txt>

module Network.Parser.Rfc5322 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad                    (join)
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString                  (ByteString, cons, pack,
                                                   singleton)
import qualified Data.ByteString.Char8            as BSC
import           Data.Char                        (digitToInt)
import           Data.Monoid
import           Data.Time
import           Data.Word                        (Word8)
import           Prelude                          hiding (id, take, takeWhile)
--------------------------------------------------------------------------------
import qualified Network.Parser.Rfc2822           as R2822
import           Network.Parser.Rfc5234
import           Network.Parser.RfcCommon         hiding (ctext, text)
--------------------------------------------------------------------------------
-- Data Types
data Message
  = Message { messageFields :: [Field]
            , messageBody   :: Maybe ByteString
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

-- 3.2.  Lexical Tokens
-- | * 3.2.1.  Quoted characters
quoted_pair :: Parser ByteString
quoted_pair = (\a b -> pack $ a:b:[]) <$> word8 92 <*> (vchar <|> wsp)

-- | * 3.2.2.  Folding White Space and Comments
-- Parse Folding Whitespace
fws :: Parser ByteString
fws = do
  _ <- option [] (many wsp *> (asList crlf))
  _ <- many1 wsp
  return " "

-- Parse ctext
ctext :: Parser Word8
ctext = satisfy (\w ->
                  w >= 33 && w <= 39
                  || (w >= 42 && w <= 91)
                  || (w >= 93 && w <= 126))

ccontent :: Parser ByteString
ccontent = (singleton <$> ctext) <|> quoted_pair <|> comment

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
                      <|> asList fws)

-- | * 3.2.3. Atom
atext :: Parser Word8
atext = alpha <|> digit <|> satisfy p
  where p = inClass "!#$%&'*+/=?^_`{|}~-"

atom :: Parser ByteString
atom = ocfws *> (pack <$> many1 atext) <* ocfws

dot_atom_text :: Parser ByteString
dot_atom_text = do
  t1 <- many1 atext
  t2 <- many ((:) <$> word8 46 <*> many1 atext)
  return (pack t1 <> pack (join t2))

dot_atom :: Parser ByteString
dot_atom = ocfws *> dot_atom_text <* ocfws

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
qcontent = pack <$> (asList qtext <|> quotedPair)

-- | * 3.2.5.  Miscellaneous Tokens
word :: Parser ByteString
word = atom <|> R2822.quoted_string

phrase :: Parser [ByteString]
phrase = many1 word

-- >>> parse unstructured "asda sdasd asd asd\n    asdasd asd\n\n"
-- Done "\n\n" "asda sdasd asd asd asdasd asd"
unstructured :: Parser ByteString
unstructured = do
  r1 <- many ((<>) <$> option mempty fws <*> (singleton <$> vchar))
          <* many wsp
  return (BSC.concat r1)

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
date_time :: Parser UTCTime
date_time = do
  _ <- option 0 (dayOfWeek <* AC.char ',')
  ((d,m,y), (tod,tz)) <- (,) <$> date <*> time <* ocfws
  let dt' = fromGregorianValid (toInteger y) m d
  case dt' of
    Just dt -> return . zonedTimeToUTC $ ZonedTime (LocalTime dt tod) tz
    Nothing -> fail "invalid day"

dayOfWeek :: Parser Int
dayOfWeek = option mempty fws *> dayName

dayName :: Parser Int
dayName
  =   AC.stringCI "Mon" *> return 0
  <|> AC.stringCI "Tue" *> return 1
  <|> AC.stringCI "Wed" *> return 2
  <|> AC.stringCI "Thu" *> return 3
  <|> AC.stringCI "Fri" *> return 4
  <|> AC.stringCI "Sat" *> return 5
  <|> AC.stringCI "Sun" *> return 6
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

-- >>> parseOnly year "2015"
-- Right 2015
year :: Parser Int
year =  conv <$> (fws *> count 4 AC.digit <* fws)
  where conv = sum . map (uncurry (*)) . zip [1000, 100, 10, 1] . fmap digitToInt

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
address = asList mailbox <|> group

mailbox :: Parser NameAddress
mailbox = name_addr <|> (NameAddress Nothing <$> addr_spec)

name_addr :: Parser NameAddress
name_addr = NameAddress <$> option Nothing (Just <$> display_name)
                        <*> angle_addr

angle_addr :: Parser ByteString
angle_addr = ocfws *> word8 60 *> addr_spec <* word8 62 <* ocfws

group :: Parser [NameAddress]
group = display_name *> word8 58 *> option [] group_list <* word8 59 <* ocfws

display_name :: Parser ByteString
display_name = BSC.unwords <$> phrase

mailbox_list :: Parser [NameAddress]
mailbox_list = (:) <$> mailbox <*> many (AC.char ',' *> mailbox)

address_list :: Parser [NameAddress]
address_list = (++) <$> address <*> (join <$> many (AC.char ',' *> address))

group_list :: Parser [NameAddress]
group_list = mailbox_list <|> (cfws *> pure [])

-- | 3.4.1.  Addr-Spec Specification
addr_spec :: Parser ByteString
addr_spec = ret <$> local_part <* AC.char '@' <*> domain
  where ret l r = l <> "@" <> r

local_part :: Parser ByteString
local_part = dot_atom <|> R2822.quoted_string

domain :: Parser ByteString
domain = dot_atom <|> domain_literal

domain_literal :: Parser ByteString
domain_literal
  = do _ <- ocfws >> word8 91
       res <- BSC.concat <$> many ((<>) <$> option mempty fws *> dcontent)
       _ <- word8 92 >> ocfws
       return ("[" <> res <> "]")

dcontent :: Parser ByteString
dcontent = (pack <$> many1 dtext) <|> quoted_pair

dtext :: Parser Word8
dtext = satisfy p
  where p w = (w>=33 && w<=90) || (w>=94 && w<= 126)

-- 3.5.  Overall Message Syntax
-- >>> parse message "Content-Type: text/plain\n\nThis is a multi\nline message.\n\0"
message :: Parser Message
message = Message <$> (fields <* crlf) <*> (option Nothing (Just <$> body))

-- (*(*998text CRLF) *998text)
-- >>> parse body "asdasd\nasdasd\n\0"
-- Done "\NUL" "asdasd\nasdasd\n"
body :: Parser ByteString
body = BSC.intercalate "\n" <$> (text998 `sepBy` crlf)

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

fields :: Parser [Field]
fields
  = many (   TraceField <$> trace
         <|> resent_date
         <|> resent_from
         <|> resent_sender
         <|> resent_to
         <|> resent_cc
         <|> resent_bcc
         <|> resent_msg_id
         <|> orig_date
         <|> from
         <|> sender
         <|> reply_to
         <|> to
         <|> cc
         <|> bcc
         <|> message_id
         <|> in_reply_to
         <|> references
         <|> subject
         <|> comments
         <|> keywords
         <|> optional_field
         )

-- | Used to generate arbitrary header parsers
header :: ByteString -> Parser a -> Parser a
header key p = AC.stringCI (key <> ":") *> p <* crlf

-- | 3.6.1. The origination date field
orig_date :: Parser Field
orig_date = header "Date" (Date <$> date_time)

-- | 3.6.2. Originator fields
from :: Parser Field
from = header "From" (From <$> mailbox_list)

sender :: Parser Field
sender = header "Sender" (Sender <$> mailbox)

reply_to :: Parser Field
reply_to = header "Reply To" (ReplyTo <$> address_list)

-- | 3.6.3. Destination address fields
to :: Parser Field
to = header "To"  (To <$> address_list)

cc :: Parser Field
cc = header "Cc"  (Cc <$> address_list)

bcc :: Parser Field
bcc = header "Bcc" (Bcc <$> option [] (address_list <|> (cfws *> return [])))

-- | 3.6.4. Identification fields
message_id :: Parser Field
message_id  = header "Message-ID"  (MessageID <$> msg_id)

in_reply_to :: Parser Field
in_reply_to = header "In-Reply-To" (InReplyTo <$> many1 msg_id)

references :: Parser Field
references  = header "References"  (References <$> many1 msg_id)

msg_id :: Parser ByteString
msg_id = ocfws *> word8 60 *> mid <* word8 62 <* ocfws
  where
    mid = res <$> id_left <* word8 64 <*> id_right
    res l r = l <> "@" <> r

id_left :: Parser ByteString
id_left = dot_atom_text

id_right :: Parser ByteString
id_right = dot_atom_text <|> no_fold_literal

no_fold_literal :: Parser ByteString
no_fold_literal = fst <$> match ("[" >> many dtext >> "]")

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
      return $ x ++ join xs

-- | 3.6.6. Resent fields
resent_date, resent_from,resent_sender, resent_to, resent_cc, resent_bcc, resent_msg_id :: Parser Field
resent_date   = header "Resent-Date"       (ResentDate      <$> date_time)
resent_from   = header "Resent-From"       (ResentFrom      <$> mailbox_list)
resent_sender = header "Resent-Sender"     (ResentSender    <$> mailbox)
resent_to     = header "Resent-To"         (ResentTo        <$> address_list)
resent_cc     = header "Resent-Cc"         (ResentCc        <$> address_list)
resent_bcc    = header "Resent-Bcc"        (ResentBcc       <$> option [] (address_list <|> (cfws *> return [])))
resent_msg_id = header "Resent-Message-ID" (ResentMessageID <$> msg_id)

-- | 3.6.7. Trace fields
trace :: Parser Trace
trace = Trace <$> option Nothing (Just <$> return_path)
              <*> many1 received

return_path :: Parser ByteString
return_path = header "Return-Path" path

path :: Parser ByteString
path = angle_addr <|> (ocfws >> AC.char '<' >> ocfws >> AC.char '>' >> ocfws >> pure "<>")

received :: Parser Received
received = header "Received" (Received <$> (many received_token <* AC.char ';') <*> (date_time <?> "date-time"))

received_token :: Parser ByteString
received_token = word <|> angle_addr <|> addr_spec <|> domain

-- | 3.6.8. Optional fields
optional_field :: Parser Field
optional_field = OptionalField <$> (field_name <* word8 58) <*> (unstructured <* crlf)

field_name :: Parser ByteString
field_name = pack <$> many1 ftext

ftext :: Parser Word8
ftext = satisfy $ \w -> (w >= 33 && w <= 57) || (w >= 59 && w <= 126)

-- | Utils
ocfws :: Parser ByteString
ocfws = option mempty cfws
