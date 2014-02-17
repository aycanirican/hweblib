{-# LANGUAGE OverloadedStrings #-}

-- | Augmented BNF for Syntax Specifications: ABNF
-- <http://www.ietf.org/rfc/rfc2234.txt>

module Network.Parser.Rfc2234 where
--------------------------------------------------------------------------------
import           Control.Applicative hiding (many)
import           Data.Attoparsec
import           Data.Word           (Word8)
--------------------------------------------------------------------------------
--
-- * Primitive Parsers (6.1 Core Rules)
--

vcharPred, spPred, lfPred, htPred
 , hexdigPred, digitPred, dquotePred
 , ctlPred, charPred, upalphaPred
 , loalphaPred, alphaPred, bitPred
    :: Word8 -> Bool

-- | Parse a Space or Horizontal Tab
wsp :: Parser Word8
wsp = sp <|> ht
{-# INLINE wsp #-}

vcharPred w = w >= 0x21 && w <= 0x7e
-- | Parse a Visible Character
vchar :: Parser Word8
vchar = satisfy vcharPred
{-# INLINE vchar #-}

spPred = (== 32)
-- | Parse a Space
sp :: Parser Word8
sp = word8 32 <?> "space"
{-# INLINE sp #-}

-- | Parse an Octet
octet :: Parser Word8
octet = anyWord8
{-# INLINE octet #-}

-- | Parse a Lightweight Space
lwsp :: Parser [Word8]
lwsp = many' (wsp <|> crlf *> wsp) <?> "lightweight space"
{-# INLINE lwsp #-}

lfPred = (== 10)
-- | Parse a LineFeed
lf :: Parser Word8
lf = word8 10 <?> "linefeed"
{-# INLINE lf #-}


htPred = (== 9)
-- | Parse a Horizontal Tab
ht :: Parser Word8
ht = word8 9 <?> "horizontal tab"
{-# INLINE ht #-}

hexdigPred w = (w >= 65 && w <= 70)
             || (w >= 97 && w <= 102)
             || (w >= 48 && w <= 57)
-- | Parse a hex digit
hexdig :: Parser Word8
hexdig = satisfy hexdigPred
{-# INLINE hexdig #-}

digitPred w = w >= 48 && w <= 57
-- | Parse a digit
digit :: Parser Word8
digit = satisfy digitPred
{-# INLINE digit #-}

dquotePred = (== 34)
-- | Parse a double quote
dquote :: Parser Word8
dquote = word8 34 <?> "double-quote"
{-# INLINE dquote #-}

ctlPred w = (w == 127) || (w >= 0) && (w < 32)
-- | Parse an ascii control character
ctl :: Parser Word8
ctl = satisfy ctlPred <?> "ascii control character"
{-# INLINE ctl #-}

-- | Parse CRLF
crlf :: Parser Word8
crlf = return 10 <$> (try (cr *> lf) <|> lf)
{-# INLINE crlf #-}

-- | Parse CR
cr :: Parser Word8
cr = word8 13 <?> "carriage return"
{-# INLINE cr #-}

charPred w = w >= 0 || w <= 127
-- | Parse a character
char :: Parser Word8
char = satisfy charPred
{-# INLINE char #-}

bitPred w = w == 48 || w == 49
-- | Parse a Bit
bit :: Parser Word8
bit = satisfy bitPred
{-# INLINE bit #-}

upalphaPred w = w >= 65 && w <= 90
-- | Parse an uppercase alpha
upalpha :: Parser Word8
upalpha = satisfy upalphaPred
{-# INLINE upalpha #-}

loalphaPred w = w >= 97 && w <= 122
-- | Parse a lowercase alpha
loalpha :: Parser Word8
loalpha = satisfy loalphaPred
{-# INLINE loalpha #-}

alphaPred w = upalphaPred w || loalphaPred w
-- | Parse an alpha
alpha :: Parser Word8
alpha = satisfy alphaPred
{-# INLINE alpha #-}

-- | Match a parser at least @N@ times.
manyN :: Int -> Parser a -> Parser [a]
manyN n p
    | n <= 0    = return []
    | otherwise = (++) <$> count n p <*> many' p
{-# INLINE manyN #-}

-- | Match a parser at least @N@ times, but no more than @M@ times.
manyNtoM :: Int -> Int -> Parser a -> Parser [a]
manyNtoM n m p
    | n <  0    = return []
    | n >  m    = return []
    | n == m    = count n p
    | n == 0    = foldr (<|>) (return []) (map (\x -> try $ count x p) (Prelude.reverse [1..m]))
    | otherwise = (++) <$> count n p <*> manyNtoM 0 (m - n) p
{-# INLINE manyNtoM #-}
