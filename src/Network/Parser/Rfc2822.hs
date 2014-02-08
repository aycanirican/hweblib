{-# LANGUAGE OverloadedStrings #-}

-- | Internet Message Format
-- <http://www.ietf.org/rfc/rfc2822.txt>

module Network.Parser.Rfc2822 where
--------------------------------------------------------------------------------
import Control.Monad (join)
import Control.Applicative as A hiding (many)
import Data.Attoparsec
import qualified Data.Attoparsec.Char8 as AC
import Data.ByteString as W hiding (concat,intersperse, group)
import Data.ByteString.Char8 as C hiding (concat,intersperse,group)
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import qualified Data.Map as M
import Prelude hiding (id)
import Data.List hiding (group)
--------------------------------------------------------------------------------
import Network.Parser.RfcCommon hiding (ctext)
import Network.Parser.Rfc2234
--------------------------------------------------------------------------------
-- | * 3.2.1. Primitive Tokens
no_ws_ctl_pred w = w == 32 || ctl_pred w
no_ws_ctl = satisfy no_ws_ctl_pred

-- | Parse a text element and return corresponding Word8
text = satisfy $ \w ->
       (w >= 1 && w<=9)
       || w == 11
       || w == 12
       || (w >= 14 && w<=127)

-- Prelude.map Data.Char.ord "()<>[]:;@\\,.\""

-- specialsSet ::[Word8]
-- specialsSet = [40,41,60,62,91,93,58,59,64,92,44,46,34]

specials_pred :: Word8 -> Bool
specials_pred = inClass "()<>[]:;@\\,.\"" -- F.memberWord8 w (F.fromList specialsSet)

-- | Parse a special
specials :: Parser Word8
specials = satisfy specials_pred

-- | * 3.2.3. Folding white space and comments

-- | Parse Whitespaces
wsps :: Parser [Word8]
wsps = many1 wsp

-- | Parse Folding Whitespace
fws :: Parser [Word8]
fws = return [32] <$> many1 (choice [wsps, crlf *> wsps])

-- | Parse ctext
ctext :: Parser Word8
ctext = crlf <|> no_ws_ctl <|> satisfy rest
    where 
      rest w = (w >= 33 && w <= 39)
               || (w >= 42 && w <= 91)
               || (w >= 93 && w <= 126)

-- | Parse a comment 
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
atext_pred w = char_pred w && not (ctl_pred w || sp_pred w || specials_pred w)
atext = satisfy atext_pred

atom :: Parser [Word8]
atom = option [] cfws *> many1 atext <* option [] cfws

dot_atom_text :: Parser [Word8]
dot_atom_text = Data.List.intercalate [46] <$> sepBy (many1 atext) (word8 46)

dot_atom :: Parser [Word8]
dot_atom = option [] cfws *> dot_atom_text <* option [] cfws

-- | * 3.2.5. Quoted strings
qtext_pred :: Word8 -> Bool
qtext_pred w = no_ws_ctl_pred w 
               || w == 33 
               || (w >= 35 && w <= 91)
               || (w >= 93 && w <= 126)

qtext :: Parser Word8
qtext = satisfy qtext_pred
{-# INLINE qtext #-}

qcontent = option [] (asList qtext) <|> quotedPair
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

-- | * 3.4. Address Specification
address :: Parser [NameAddress]
address = try (asList mailbox)
          <|> group

mailbox :: Parser NameAddress
mailbox = try name_addr
          <|> do a <- addr_spec
                 return $ NameAddress Nothing (W.pack a)

name_addr :: Parser NameAddress
name_addr = do n <- option [] display_name
               a <- angle_addr
               return $ if Data.List.null n
                          then NameAddress Nothing (W.pack a)
                          else NameAddress (Just . W.pack $ n) (W.pack a)

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

dtext_pred w = no_ws_ctl_pred w || (w>=33 && w<=90) || (w>=94 && w<= 126)
dtext = satisfy dtext_pred

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
