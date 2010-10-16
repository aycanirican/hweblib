{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  #-}

module Network.Http.Parser.Rfc2616 where

import Control.Applicative hiding (many)
import Data.Attoparsec as AW
import Data.Attoparsec.Char8 as AC hiding (digit, char)
import qualified Data.Attoparsec.Char8 as DAC
import qualified Data.Attoparsec.FastSet as AF
import Data.ByteString as W
import Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8, Word64)
import Data.Char (digitToInt, isAsciiUpper, isAsciiLower)
import Prelude hiding (take, takeWhile)

type HttpVersion = (Int,Int)

data Method  = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT
               deriving(Show,Read,Ord,Eq)

-- | Basic Parser Constructs for RFC 2616

char_pred, upalpha_pred
 , loalpha_pred, alpha_pred, digit_pred, ctl_pred
 , cr_pred, lf_pred, sp_pred, ht_pred, dquote_pred
 , separators_pred, token_pred, hex_pred
 :: Word8 -> Bool

-- parse octet (C.pack "abc") => 97
octet :: Parser Word8
octet = anyWord8
{-# INLINE octet #-}

-- parse Rfc2616.char (C.pack "abc") => 'a'
char_pred w = w >= 0 || w <= 127
char :: Parser Word8
char = AW.satisfy char_pred
{-# INLINE char #-}

upalpha_pred w = w >= 65 && w <= 90
upalpha :: Parser Word8
upalpha = AW.satisfy upalpha_pred
{-# INLINE upalpha #-}

loalpha_pred w = w >= 97 && w <= 122
loalpha :: Parser Word8
loalpha = AW.satisfy loalpha_pred
{-# INLINE loalpha #-}

alpha_pred w = upalpha_pred w || loalpha_pred w
alpha :: Parser Word8
alpha = AW.satisfy alpha_pred
{-# INLINE alpha #-}

digit_pred w = w >= 48 && w <= 57 
digit :: Parser Word8
digit = AW.satisfy digit_pred
{-# INLINE digit #-}

ctl_pred w = (w == 127) || (w >= 0) && (w < 32)
ctl :: Parser Word8
ctl = AW.satisfy ctl_pred <?> "ascii control character"
{-# INLINE ctl #-}

cr_pred = (== 13)
cr :: Parser Word8
cr = word8 13 <?> "carriage return"
{-# INLINE cr #-}

lf_pred = (== 10)
lf :: Parser Word8
lf = word8 10 <?> "linefeed"
{-# INLINE lf #-}

sp_pred = (== 32)
sp :: Parser Word8
sp = word8 32 <?> "space"
{-# INLINE sp #-}

ht_pred = (== 9)
ht :: Parser Word8
ht = word8 9 <?> "horizontal tab"
{-# INLINE ht #-}

dquote_pred = (== 34)
dquote :: Parser Word8
dquote = word8 34 <?> "double-quote"
{-# INLINE dquote #-}

crlf :: Parser Word8
crlf = try (cr *> lf) <|> lf <?> "crlf or lf"
{-# INLINE crlf #-}

-- parse lws (C.pack "\n\t  asd")
lws :: Parser Word8
lws = (try (crlf *> s) <|> s) *> return 32 <?> "lightweight space"
  where s = many1 (sp <|> ht)
{-# INLINE lws #-}

text :: Parser Word8
text = crlf <|> AW.satisfy char_not_ctl
  where char_not_ctl w = char_pred w && not (ctl_pred w)
{-# INLINE text #-}

hex_pred w = (w >= 65 && w <= 70) 
             || (w >= 97 && w <= 102)
             || (w >= 48 && w <= 57)
hex :: Parser [Word8]
hex = many1 $ AW.satisfy hex_pred
{-# INLINE hex #-}

token_pred w = char_pred w && not (ctl_pred w || separators_pred w)
token :: Parser [Word8]
token = many1 $ AW.satisfy token_pred
{-# INLINE token #-}

separatorSet :: [Word8]
separatorSet = [40,41,60,62,64,44,59,58,92,34,47,91,93,63,61,123,125,32,9]
separators_pred w = AF.memberWord8 w (AF.fromList separatorSet)
separators :: Parser Word8
separators = AW.satisfy separators_pred
{-# INLINE separators #-}

ctext :: Parser Word8
ctext = crlf <|> AW.satisfy char_not_ctl_or_paren
  where 
    char_not_ctl_or_paren w 
        = char_pred w && not (w == 40 || w == 41) && not (ctl_pred w)
{-# INLINE ctext #-}

qdtext :: Parser Word8
qdtext = crlf <|> AW.satisfy char_not_ctl_or_dquote
  where 
    char_not_ctl_or_dquote w 
        = char_pred w && not (dquote_pred w) && not (ctl_pred w)

quotedPair :: Parser Word8
quotedPair = word8 92 *> char

quotedString :: Parser [Word8]
quotedString = word8 34 *> many (quotedPair <|> qdtext) <* word8 34

comment :: Parser [Word8]
comment = word8 40 *> many (quotedPair <|> ctext) <* word8 41

-- parse (httpVersion) (W.pack "HTTP/12.15\n")
httpVersion :: Parser HttpVersion
httpVersion = string "HTTP/" *> 
              ((,) <$> (num <* sep) <*> num)
 where num = many1 digit >>= return . read . C.unpack . W.pack
       sep = word8 . c2w $ '.'


-- parse (httpMethod) (W.pack "GET /")
httpMethod :: Parser Method
httpMethod =  (GET     <$ string "GET")
          <|> (PUT     <$ string "PUT")
          <|> (POST    <$ string "POST")
          <|> (HEAD    <$ string "HEAD")
          <|> (DELETE  <$ string "DELETE")
          <|> (TRACE   <$ string "TRACE")
          <|> (CONNECT <$ string "CONNECT")
          <|> (OPTIONS <$ string "OPTIONS")

