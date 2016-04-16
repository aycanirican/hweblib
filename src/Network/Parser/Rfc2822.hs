{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.Rfc2822
-- Copyright   :  Aycan iRiCAN 2010-2015
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
import           Data.ByteString                  (ByteString, pack)
import           Data.Char                        (digitToInt)
import           Data.Fixed                       (Fixed (MkFixed))
import           Data.List                        (intercalate)
import           Data.Time
import           Data.Word                        (Word8)
import           Prelude                          hiding (id, take, takeWhile)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2234
import           Network.Parser.RfcCommon         hiding (ctext)
--------------------------------------------------------------------------------

-- | * 3.2.1. Primitive Tokens
no_ws_ctlPred w = w == 32 || ctlPred w
no_ws_ctl = satisfy no_ws_ctlPred

-- Parse a text element and return corresponding Word8
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

-- | * 3.2.3. Folding white space and comments

-- Parse Whitespaces
wsps :: Parser [Word8]
wsps = many1 wsp

-- Parse Folding Whitespace
fws :: Parser [Word8]
fws = return [32] <$> many1 (choice [wsps, crlf *> wsps])

-- Parse ctext
ctext :: Parser Word8
ctext = crlf <|> no_ws_ctl <|> satisfy rest
    where
      rest w = (w >= 33 && w <= 39)
               || (w >= 42 && w <= 91)
               || (w >= 93 && w <= 126)

-- Parse a comment
comment :: Parser [Word8]
comment = do
  word8 40
  r1 <- many' ccontent
  r2 <- option [] fws
  word8 41
  return $ join r1 ++ r2
    where
      ccontent :: Parser [Word8]
      ccontent = try $ do r1 <- option [] fws
                          r2 <- choice [many1 ctext, quotedPair, comment]
                          return $ r1 ++ r2

cfws = concat <$> many1 (choice [fws, comment])

-- | * 3.2.4. Atom
atextPred w = charPred w && not (ctlPred w || spPred w || specialsPred w)
atext = satisfy atextPred

atom :: Parser [Word8]
atom = option [] cfws *> many1 atext <* option [] cfws

dot_atom_text :: Parser [Word8]
dot_atom_text = Data.List.intercalate [46] <$> sepBy (many1 atext) (word8 46)

dot_atom :: Parser [Word8]
dot_atom = option [] cfws *> dot_atom_text <* option [] cfws

-- | * 3.2.5. Quoted strings
qtextPred :: Word8 -> Bool
qtextPred w = no_ws_ctlPred w
               || w == 33
               || (w >= 35 && w <= 91)
               || (w >= 93 && w <= 126)

qtext :: Parser Word8
qtext = satisfy qtextPred
{-# INLINABLE qtext #-}

qcontent :: Parser [Word8]
qcontent = option [] (asList qtext) <|> quotedPair

quoted_string :: Parser [Word8]
quoted_string = do
  option [] cfws
  dquote
  r1 <- concat <$> many' (do
                          r1 <- option [] fws
                          r2 <- qcontent
                          return (r1 ++ r2))
  r2 <- option [] fws
  dquote
  option [] cfws
  return $ [34] ++ r1 ++ r2 ++ [34]

-- | * 3.2.6. Miscellaneous tokens
word = atom <|> quotedString

phrase :: Parser [[Word8]]
phrase = many1 word
utext = no_ws_ctl <|> satisfy (\w -> w>=33 && w<=126)

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

-- >>> parseOnly dateTime "Mon, 21 Sep 1980 10:01:02 +0230"
-- >>> Right 1980-09-21 07:31:02 UTC
dateTime :: Parser UTCTime
dateTime = do
  option 0 (dayOfWeek <* AC.char ',')
  ((d,m,y), (tod,tz)) <- (,) <$> date <* option [] fws <*> time
  let dt' = fromGregorianValid (toInteger y) m d
  case dt' of
    Just dt -> return . zonedTimeToUTC $ ZonedTime (LocalTime dt tod) tz
    Nothing -> fail "invalid day"

dayOfWeek :: Parser Int
dayOfWeek = option [] fws *> dayName

dayName :: Parser Int
dayName
  =   string "Mon" *> return 0
  <|> string "Tue" *> return 1
  <|> string "Wed" *> return 2
  <|> string "Thu" *> return 3
  <|> string "Fri" *> return 4
  <|> string "Sat" *> return 5
  <|> string "Sun" *> return 6

-- >>> parseOnly date "21 Sep 1980"
-- Right (21,9,1980)
date :: Parser (Int, Int, Int)
date = (,,) <$> day <*> month <*> year

-- >>> parseOnly year "2015"
-- Right 2015
year :: Parser Int
year = sum . map (uncurry (*)) . zip [1000, 100, 10, 1] . fmap digitToInt <$> count 4 AC.digit

-- >>> parseOnly month " Feb"
-- Right 2
month :: Parser Int
month = option [] fws *> monthName <* option [] fws

monthName :: Parser Int
monthName
  =   string "Jan" *> return 1
  <|> string "Feb" *> return 2
  <|> string "Mar" *> return 3
  <|> string "Apr" *> return 4
  <|> string "May" *> return 5
  <|> string "Jun" *> return 6
  <|> string "Jul" *> return 7
  <|> string "Aug" *> return 8
  <|> string "Sep" *> return 9
  <|> string "Oct" *> return 10
  <|> string "Nov" *> return 11
  <|> string "Dec" *> return 12

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
    Left  l -> minutesToTimeZone . negate . minutes <$> fourDigit
    Right r -> minutesToTimeZone . minutes <$> fourDigit
  where
    minutes (a,b,c,d) = (((a*10)+b) * 60) + ((c*10) + d)
    fourDigit :: Parser (Int, Int, Int, Int)
    fourDigit = (\[a,b,c,d] -> (a,b,c,d)) . map digitToInt <$> count 4 AC.digit


-- dateTime :: Parser UTCTime
-- dateTime = do
--   dow <- try (dayOfWeek)

-- | * 3.4. Address Specification
address :: Parser [NameAddress]
address = try (asList mailbox)
          <|> group

mailbox :: Parser NameAddress
mailbox = try name_addr
          <|> do a <- addr_spec
                 return $ NameAddress Nothing (pack a)

name_addr :: Parser NameAddress
name_addr = do n <- option [] display_name
               a <- angle_addr
               return $ if null n
                          then NameAddress Nothing (pack a)
                          else NameAddress (Just . pack $ n) (pack a)

angle_addr :: Parser [Word8]
angle_addr = do
  option [] cfws
  word8 60
  a <- addr_spec
  word8 62
  option [] cfws
  return a

group :: Parser [NameAddress]
group = do
  display_name
  word8 58
  r <- option [] mailbox_list
  option [] cfws
  word8 59
  option [] cfws
  return r

display_name = ret <$> phrase
    where ret = Data.List.intercalate [32]

mailbox_list = sepBy mailbox (word8 44)
address_list = sepBy address (word8 44)

addr_spec :: Parser [Word8]
addr_spec = ret <$> local_part <*> word8 64 <*> domain
    where ret l m r = l ++ [m] ++ r

local_part = dot_atom <|> quotedString
domain = dot_atom <|> domain_literal
domain_literal = do
  option [] cfws
  word8 91
  r <- many' (option [] fws *> dcontent)
  word8 92
  return $ [91] ++ concat r ++ [92]
dcontent = try (do r <- dtext
                   return [r])
           <|> quotedPair

dtextPred w = no_ws_ctlPred w || (w>=33 && w<=90) || (w>=94 && w<= 126)
dtext = satisfy dtextPred

-- | * 3.6.4. Identification fields

message_id = AC.stringCI "message-id:" *> msg_id

msg_id = do
  option [] cfws
  word8 60
  r <- res <$> id_left <*> word8 64 <*> id_right
  word8 62
  option [] cfws
  return r
    where res l m r = l ++ [m] ++ r

id_left :: Parser [Word8]
id_left = dot_atom_text <|> no_fold_quote

id_right :: Parser [Word8]
id_right = dot_atom_text <|> no_fold_literal

no_fold_quote = do
  l <- dquote
  m <- concat <$> many' (option [] (asList qtext) <|> quotedPair)
  r <- dquote
  return $ [l] ++ m ++ [r]

no_fold_literal = do
  l <- word8 91 -- '['
  m <- concat <$> many' (option [] (asList dtext) <|> quotedPair)
  r <- word8 93 -- ']'
  return $ [l] ++ m ++ [r]

-- * ADTs
data NameAddress
    = NameAddress
      { naName :: Maybe ByteString
      , naAddr :: ByteString
      } deriving (Eq, Show)
