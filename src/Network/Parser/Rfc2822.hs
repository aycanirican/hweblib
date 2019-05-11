{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.Rfc2822
-- Copyright   :  Aycan iRiCAN 2010-2019
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Internet Message Format
-- <http://www.ietf.org/rfc/rfc2822.txt>

module Network.Parser.Rfc2822 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad                    (join)
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString                  as B (ByteString, concat,
                                                        intercalate, pack)
import qualified Data.ByteString.Char8            as BSC
import           Data.Char                        (digitToInt)
import           Data.List                        as L (concat, intercalate)
import           Data.Maybe                       (catMaybes)
import           Data.Monoid
import           Data.Time
import           Data.Word                        (Word8)
import           Prelude                          hiding (id, take, takeWhile)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2234
import           Network.Parser.RfcCommon         hiding (ctext)
--------------------------------------------------------------------------------
--  ** General Utils

-- | Used to generate arbitrary header parsers
header :: ByteString -> Parser a -> Parser a
header key p = AC.stringCI (key <> ": ") *> p <* crlf

-- ** 3.2.1. Primitive Tokens
no_ws_ctlPred :: Word8 -> Bool
no_ws_ctlPred w
  = (w >= 1 && w <= 8)
    || (w == 11) || (w == 12)
    || (w <= 14 && w >= 31)
    || w == 127

no_ws_ctl :: Parser Word8
no_ws_ctl = satisfy no_ws_ctlPred

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
fws = return [32] <$> many1 (choice [wsps, crlf *> wsps])

-- Parse ctext
ctext :: Parser Word8
ctext = no_ws_ctl <|> satisfy rest
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
      ccontent = choice [(pack <$> many1 ctext), pack <$> quotedPair, comment]

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

dot_atom_text :: Parser ByteString
dot_atom_text = pack <$> L.intercalate [46] <$> sepBy (many1 atext) (word8 46)

dot_atom :: Parser ByteString
dot_atom = ocfws *> dot_atom_text <* ocfws

-- | * 3.2.5. Quoted strings
qtextPred :: Word8 -> Bool
qtextPred w = no_ws_ctlPred w
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
-- >>> parseOnly quoted_string "\"foo is on the bar\""
-- Right "foo is on the bar"

quoted_string :: Parser ByteString
quoted_string = pack . join <$> (dquote *> many (asList qdtext <|> quotedPair) <* dquote)

-- | * 3.2.6. Miscellaneous tokens
word :: Parser ByteString
word = atom <|> quoted_string

phrase :: Parser ByteString
phrase = B.intercalate " " <$> many1 word

utext :: Parser Word8
utext = no_ws_ctl <|> satisfy (\w -> w>=33 && w<=126)

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
  _ <- option 0 (dayOfWeek <* AC.char ',')
  ((d,m,y), (tod,tz)) <- (,) <$> (date <* fws) <*> (time <* ocfws)
  let dt' = fromGregorianValid (toInteger y) m d
  case dt' of
    Just dt -> return . zonedTimeToUTC $ ZonedTime (LocalTime dt tod) tz
    Nothing -> fail "invalid day"

dayOfWeek :: Parser Int
dayOfWeek = option [] fws *> dayName

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

-- >>> parseOnly year "2015"
-- Right 2015
year :: Parser Int
year = sum . zipWith (*) [1000, 100, 10, 1] . fmap digitToInt <$> count 4 AC.digit

-- >>> parseOnly month " Feb"
-- Right 2
month :: Parser Int
month = option [] fws *> monthName <* option [] fws

monthName :: Parser Int
monthName
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
mailbox = try name_addr
      <|> NameAddress Nothing <$> addr_spec

name_addr :: Parser NameAddress
name_addr = do n <- option mempty display_name
               a <- angle_addr
               return $ if "" ==  n
                          then NameAddress Nothing a
                          else NameAddress (Just n) a

angle_addr :: Parser ByteString
angle_addr
  = ocfws *> word8 60 *> addr_spec <* word8 62 <* ocfws

group :: Parser [NameAddress]
group
  = display_name *> word8 58 *> option [] mailbox_list
      <* ocfws <* word8 59 <* ocfws

display_name :: Parser ByteString
display_name = phrase

mailbox_list :: Parser [NameAddress]
mailbox_list = sepBy mailbox (word8 44)

address_list :: Parser [NameAddress]
address_list = Prelude.concat <$> sepBy address (word8 44)

addr_spec :: Parser ByteString
addr_spec = ret <$> local_part <* AC.char '@' <*> domain
  where ret l r = l <> "@" <> r

local_part :: Parser ByteString
local_part = dot_atom <|> quoted_string

domain :: Parser ByteString
domain = dot_atom <|> (pack <$> domain_literal)

domain_literal :: Parser [Word8]
domain_literal
  =  ocfws *> word8 91
  *> ((\xs -> [91] ++ (L.concat xs) ++ [92]) <$> many' (option [] fws *> dcontent))
  <* word8 92

dcontent :: Parser [Word8]
dcontent = many1 dtext <|> quotedPair

dtextPred :: Word8 -> Bool
dtextPred w = no_ws_ctlPred w || (w>=33 && w<=90) || (w>=94 && w<= 126)
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

body :: Parser ByteString
body = BSC.pack <$> many AC.anyChar

--  ** 3.6 Field Definitions
-- | 3.6.1. The origination date field
orig_date :: Parser Field
orig_date = header "Date" (Date <$> dateTime)

-- | 3.6.2. Originator fields
from :: Parser Field
from = header "From" (From <$> mailbox_list)

sender :: Parser Field
sender = header "Sender" (Sender <$> mailbox)

reply_to :: Parser Field
reply_to = header "Reply To" (ReplyTo <$> mailbox_list)

-- | 3.6.3. Destination address fields
to :: Parser Field
to = header "To"  (To <$> address_list)

cc :: Parser Field
cc = header "Cc"  (Cc <$> address_list)

bcc :: Parser Field
bcc = header "Bcc" (Bcc <$> (address_list <|> (many cfws *> return [])))

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
id_left = dot_atom_text <|> pack <$> no_fold_quote

id_right :: Parser ByteString
id_right = dot_atom_text <|> pack <$> no_fold_literal

no_fold_quote :: Parser [Word8]
no_fold_quote = do
  l <- dquote
  m <- L.concat <$> many' (option [] (asList qtext) <|> quotedPair)
  r <- dquote
  return $ [l] ++ m ++ [r]

no_fold_literal :: Parser [Word8]
no_fold_literal = do
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
resent_date, resent_from, resent_sender, resent_to,resent_cc,resent_bcc,resent_msg_id :: Parser Field
resent_date   = header "Resent-Date"       (ResentDate      <$> dateTime)
resent_from   = header "Resent-From"       (ResentFrom      <$> mailbox_list)
resent_sender = header "Resent-Sender"     (ResentSender    <$> mailbox)
resent_to     = header "Resent-To"         (ResentTo        <$> address_list)
resent_cc     = header "Resent-Cc"         (ResentCc        <$> address_list)
resent_bcc    = header "Resent-Bcc"        (ResentBcc       <$> (address_list <|> (many cfws *> return [])))
resent_msg_id = header "Resent-Message-ID" (ResentMessageID <$> msg_id)

-- | 3.6.7. Trace fields

trace :: Parser Trace
trace = Trace <$> optional return_path
              <*> many1 received

return_path :: Parser ByteString
return_path = header "Return-Path" path

path :: Parser ByteString
path = angle_addr <|> (ocfws >> AC.char '<' >> ocfws >> AC.char '>' >> ocfws >> pure "<>")

received :: Parser Received
received = header "Received" (Received <$> (name_val_list <* AC.char ';') <*> (dateTime <?> "date-time"))

-- >>> parseOnly name_val_list "by 10.112.112.39 with SMTP id in7csp2918309lbb"
-- Right [("by","10.112.112.39"),("with","SMTP"),("id","in7csp2918309lbb")]
name_val_list :: Parser [(ByteString, ByteString)]
name_val_list = do
  _ <- ocfws
  n  <- optional name_val_pair
  ns <- many (ocfws *> (Just <$> name_val_pair))
  return . catMaybes $ n:ns

name_val_pair :: Parser (ByteString, ByteString)
name_val_pair = (,) <$> (item_name <* cfws) <*> item_value

item_name :: Parser ByteString
item_name = do
  x  <- alpha
  xs  <- many (alpha <|> digit <|> word8 45)
  return . pack $ x:xs

-- >>> parseOnly item_value "10.112.112.39"
-- Right "10.112.112.39"
item_value :: Parser ByteString
item_value = (B.concat <$> many1 angle_addr)
         <|> (addr_spec <?> "addr-spec")
         <|> (domain    <?> "domain")
         <|> (atom      <?> "atom")
         <|> (msg_id    <?> "msg-id")


-- | 3.6.8. Optional fields
-- TODO start here..
optional_field :: Parser Field
optional_field = OptionalField <$> (field_name <* word8 58) <*> (unstructured <* crlf)

field_name :: Parser ByteString
field_name = pack <$> many1 ftext

ftext :: Parser Word8
ftext = satisfy $ \w -> (w >= 33 && w <= 57) || (w >= 59 && w <= 126)

-- * ADTs
data Received
  = Received { receivedNameVals :: [(ByteString, ByteString)]
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
