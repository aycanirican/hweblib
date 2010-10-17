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
import Data.Char (digitToInt, chr, ord)
import Prelude hiding (take, takeWhile)
import qualified Network.Http.Parser.Rfc3986 as R3986
import Network.Http.Parser.RfcCommon

-- | Basic Parser Constructs for RFC 2616

char_pred, ctl_pred
 , cr_pred, lf_pred, sp_pred, ht_pred, dquote_pred
 , separators_pred, token_pred
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

-- parse lws and return space
lws :: Parser Word8
lws = (try (crlf *> s) <|> s) *> return 32 <?> "lightweight space"
  where s = many1 (sp <|> ht)
{-# INLINE lws #-}

-- consecutive matches of lws rule, where they MUST be compressed to a
-- single 0x20 byte
lwss :: Parser Word8
lwss = do many lws; return 32

text :: Parser Word8
text = crlf <|> AW.satisfy char_not_ctl
  where char_not_ctl w = char_pred w && not (ctl_pred w)
{-# INLINE text #-}

token_pred w = char_pred w && not (ctl_pred w || separators_pred w)
token :: Parser [Word8]
token = many1 $ AW.satisfy token_pred
{-# INLINE token #-}

-- "()<>@,;:\\\"/[]?={} \t"
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
httpVersion = stringCI "HTTP/" *> 
              ((,) <$> (num <* sep) <*> num)
 where num = many1 digit >>= return . read . C.unpack . W.pack
       sep = word8 . c2w $ '.'


-- parse (httpMethod) (W.pack "GET /")
method :: Parser Method
method = (GET         <$ stringCI "GET")
         <|> (PUT     <$ stringCI "PUT")
         <|> (POST    <$ stringCI "POST")
         <|> (HEAD    <$ stringCI "HEAD")
         <|> (DELETE  <$ stringCI "DELETE")
         <|> (TRACE   <$ stringCI "TRACE")
         <|> (CONNECT <$ stringCI "CONNECT")
         <|> (OPTIONS <$ stringCI "OPTIONS")
         <|> ((EXTENSIONMETHOD . W.pack) <$> token)

requestLine :: Parser (Method, R3986.URI, HttpVersion)
requestLine = ret <$> method      <* sp
                  <*> R3986.uri   <* sp
                  <*> httpVersion <* crlf
    where ret m u h = (m,u,h)

headerContentNc_pred w 
       = (w >= 0x00 && w <= 0x08)
      || (w >= 0x0b && w <= 0x0c)
      || (w >= 0x0e && w <= 0x1f)
      || (w >= 0x21 && w <= 0x39)
      || (w >= 0x3b && w <= 0xff)

headerContent = AW.satisfy (\w -> headerContentNc_pred w || w == 58) -- ':'
headerName = many1 $ AW.satisfy headerContentNc_pred
headerValue = do
  c <- headerContent
  r <- option [] (many (headerContent <|> lws))
  return (c:r)

header = ret <$> headerName <* (word8 58 <* lwss)
             <*> headerValue <* lwss
    where ret n v = (W.pack n, W.pack v)

-- request = do 
--   (m,u,v) <- requestLine
--   crlf
--   b <- option [] messageBody
--   return ()


-- data GenericMessage = GenericMessage
--     { 

-- data HttpMessage = Request | Response
data Header = GeneralHeader | RequestHeader | EntityHeader

-- data Request = Request
--     { rqMethod  :: Method
--       rqVersion :: (Int,Int)
--     , rqBody    :: Maybe ByteString 
--     }

type HttpVersion = (Int,Int)
data Method = GET | HEAD | POST | PUT | DELETE 
            | TRACE | OPTIONS | CONNECT 
            | EXTENSIONMETHOD ByteString
              deriving (Show,Read,Ord,Eq)
