{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  #-}

module Network.Http.Parser.Rfc2234 where

import Control.Applicative hiding (many)
import Data.Attoparsec
import Data.Word (Word8)

--
-- * Primitive Parsers (6.1 Core Rules)
--

vchar_pred, sp_pred, lf_pred, ht_pred
 , hexdig_pred, digit_pred, dquote_pred
 , ctl_pred, char_pred, upalpha_pred
 , loalpha_pred, alpha_pred, bit_pred
    :: Word8 -> Bool

wsp :: Parser Word8
wsp = sp <|> ht

vchar_pred w = (w >= 0x21 && w <= 0x7e)
vchar :: Parser Word8
vchar = satisfy vchar_pred

sp_pred = (== 32)
sp :: Parser Word8
sp = word8 32 <?> "space"
{-# INLINE sp #-}

octet :: Parser Word8
octet = anyWord8
{-# INLINE octet #-}

lwsp :: Parser [Word8]
lwsp = many (wsp <|> crlf *> wsp) <?> "lightweight space"
{-# INLINE lwsp #-}

lf_pred = (== 10)
lf :: Parser Word8
lf = word8 10 <?> "linefeed"
{-# INLINE lf #-}

ht_pred = (== 9)
ht :: Parser Word8
ht = word8 9 <?> "horizontal tab"
{-# INLINE ht #-}

hexdig_pred w = (w >= 65 && w <= 70) 
             || (w >= 97 && w <= 102)
             || (w >= 48 && w <= 57)
hexdig :: Parser Word8
hexdig = satisfy hexdig_pred
{-# INLINE hexdig #-}

digit_pred w = w >= 48 && w <= 57 
digit :: Parser Word8
digit = satisfy digit_pred
{-# INLINE digit #-}

dquote_pred = (== 34)
dquote :: Parser Word8
dquote = word8 34 <?> "double-quote"
{-# INLINE dquote #-}

ctl_pred w = (w == 127) || (w >= 0) && (w < 32)
ctl :: Parser Word8
ctl = satisfy ctl_pred <?> "ascii control character"
{-# INLINE ctl #-}

crlf :: Parser Word8
crlf = try (cr *> lf) <|> lf <?> "crlf or lf"
{-# INLINE crlf #-}

cr :: Parser Word8
cr = word8 13 <?> "carriage return"
{-# INLINE cr #-}

char_pred w = w >= 0 || w <= 127
char :: Parser Word8
char = satisfy char_pred
{-# INLINE char #-}

bit_pred w = w == 48 || w == 49
bit :: Parser Word8
bit = satisfy bit_pred

upalpha_pred w = w >= 65 && w <= 90
upalpha :: Parser Word8
upalpha = satisfy upalpha_pred
{-# INLINE upalpha #-}

loalpha_pred w = w >= 97 && w <= 122
loalpha :: Parser Word8
loalpha = satisfy loalpha_pred
{-# INLINE loalpha #-}

alpha_pred w = upalpha_pred w || loalpha_pred w
alpha :: Parser Word8
alpha = satisfy alpha_pred
{-# INLINE alpha #-}

-- | Match a parser at least @n@ times.
manyN :: Int -> Parser a -> Parser [a]
manyN n p
    | n <= 0    = return []
    | otherwise = (++) <$> count n p <*> many p
{-# INLINE manyN #-}

-- | Match a parser at least @n@ times, but no more than @m@ times.
manyNtoM :: Int -> Int -> Parser a -> Parser [a]
manyNtoM n m p
    | n <  0    = return []
    | n >  m    = return []
    | n == m    = count n p
    | n == 0    = do foldr (<|>) (return []) (map (\x -> try $ count x p) (Prelude.reverse [1..m]))
    | otherwise = (++) <$> count n p <*> manyNtoM 0 (m - n) p

{-# INLINE manyNtoM #-}

-- ** Useful additions

-- quotedPair :: Parser Word8
-- quotedPair = word8 92 *> char
-- {-# INLINE quotedPair #-}

-- quotedString :: Parser [Word8]
-- quotedString = word8 34 *> many (quotedPair <|> qdtext) <* word8 34
-- {-# INLINE quotedString #-}

