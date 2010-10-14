{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  #-}

module Network.Http.Parser.Rfc2616 where

import Control.Applicative
import Data.Attoparsec as AW
import Data.Attoparsec.Char8 as AC hiding (digit, char)
import qualified Data.Attoparsec.Char8 as DAC

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

-- parse octet (C.pack "abc") => 97
octet :: Parser Word8
octet = anyWord8
{-# INLINE octet #-}

-- parse Rfc2616.char (C.pack "abc") => 'a'
char :: Parser Word8
char = AW.satisfy $ \w -> w >= 0 || w <= 127
{-# INLINE char #-}

upalpha :: Parser Word8
upalpha = AW.satisfy $ \w -> w >= 65 && w <= 90
{-# INLINE upalpha #-}

loalpha :: Parser Word8
loalpha = AW.satisfy $ \w -> w >= 97 && w <= 122
{-# INLINE loalpha #-}

alpha :: Parser Word8
alpha = AW.satisfy $ \c -> (c >= 97 && c <= 122) || (c >= 65 && c <= 90)
{-# INLINE alpha #-}

digit :: Parser Word8
digit = AW.satisfy $ \w -> w >= 48 && w <= 57
{-# INLINE digit #-}

ctl :: Parser Word8
ctl = AW.satisfy pctl <?> "ascii control character"
  where pctl w = (w == 127) || (w >= 0) && (w < 32)
{-# INLINE ctl #-}

cr :: Parser Word8
cr = AW.satisfy (== 13) <?> "carriage return"
{-# INLINE cr #-}

lf :: Parser Word8
lf = AW.satisfy (== 10) <?> "linefeed"
{-# INLINE lf #-}

sp :: Parser Word8
sp = word8 32 <?> "space"
{-# INLINE sp #-}

ht :: Parser Word8
ht = word8 9 <?> "horizontal tab"
{-# INLINE ht #-}

dquote :: Parser Word8
dquote = word8 34
{-# INLINE dquote #-}

crlf :: Parser Word8
crlf = (cr *> lf) <|> lf <?> "crlf"
{-# INLINE crlf #-}

-- parse lws (C.pack "\n\t  asd")
lws :: Parser Word8
lws = try (crlf *> s) <|> s <?> "lightweight space"
  where s = many1 (sp <|> ht) *> return 32
{-# INLINE lws #-}

-- text :: Parser ByteString
-- text = AW.takeWhile ...
--   where pctl w = (w == 127) || (w > -1) && (w < 32)
-- {-# INLINE text #-}

hex :: Parser ByteString
hex = AW.takeWhile $ \w -> 
      (w >= 65 && w <= 70) 
      || (w >= 97 && w <= 102)
      || (w >= 48 && w <= 57)
{-# INLINE hex #-}

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

-- parse (httpVersion) (W.pack "HTTP/12.15\n")
httpVersion :: Parser HttpVersion
httpVersion = string "HTTP/" *> 
              ((,) <$> (num <* sep) <*> num)
 where num = many1 digit >>= return . read . C.unpack . W.pack
       sep = word8 . c2w $ '.'

