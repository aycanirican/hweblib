{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.Rfc5234
-- Copyright   :  Aycan iRiCAN 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Augmented BNF for Syntax Specifications: ABNF
--
-- <http://www.ietf.org/rfc/rfc5234.txt>

module Network.Parser.Rfc5234 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Word                  (Word8)
--------------------------------------------------------------------------------

-- | Appendix B.1  Core ABNF of ABNF

vcharPred, spPred, lfPred, htabPred
 , hexdigPred, digitPred, dquotePred
 , ctlPred, charPred, alphaPred, bitPred
    :: Word8 -> Bool

-- | Parse a Space or Horizontal Tab
wsp :: Parser Word8
wsp = sp <|> htab
{-# INLINABLE wsp #-}

vcharPred w = w >= 0x21 && w <= 0x7e
-- | Parse a Visible Character
vchar :: Parser Word8
vchar = satisfy vcharPred
{-# INLINABLE vcharPred #-}
{-# INLINABLE vchar #-}

spPred = (== 0x20)
-- | Parse a Space
sp :: Parser Word8
sp = satisfy spPred <?> "space"
{-# INLINABLE sp #-}

-- | Parse an Octet
octet :: Parser Word8
octet = anyWord8
{-# INLINABLE octet #-}

-- | Parse a Lightweight Space
lwsp :: Parser [Word8]
lwsp = many (wsp <|> (crlf *> wsp)) <?> "lightweight space"
{-# INLINABLE lwsp #-}

lfPred = (== 0x0a)
-- | Parse a LineFeed
lf :: Parser Word8
lf = satisfy lfPred <?> "linefeed"
{-# INLINABLE lf #-}

htabPred = (== 0x9)
-- | Parse a Horizontal Tab
htab :: Parser Word8
htab = satisfy htabPred <?> "horizontal tab"
{-# INLINABLE htabPred #-}
{-# INLINABLE htab #-}

hexdigPred w
  =    (w >= 65 && w <= 70) -- A..F
    || (w >= 97 && w <= 102) -- a..f
    || (w >= 48 && w <= 57) -- 0..9
-- | Parse a hex digit
hexdig :: Parser Word8
hexdig = satisfy hexdigPred
{-# INLINABLE hexdig #-}

dquotePred = (== 0x22)
-- | Parse a double quote
dquote :: Parser Word8
dquote = satisfy dquotePred <?> "double-quote"
{-# INLINABLE dquotePred #-}
{-# INLINABLE dquote #-}

digitPred w = w >= 0x30 && w <= 0x39
-- | Parse a digit
digit :: Parser Word8
digit = satisfy digitPred
{-# INLINABLE digitPred #-}
{-# INLINABLE digit #-}

ctlPred w = (w == 0x7f) || (w >= 0) && (w < 0x1f)
-- | Parse an ascii control character
ctl :: Parser Word8
ctl = satisfy ctlPred <?> "ascii control character"
{-# INLINABLE ctl #-}

-- | Parse CRLF
crlf :: Parser Word8
crlf = return 10 <$> (try (cr *> lf) <|> lf)
{-# INLINABLE crlf #-}

-- | Parse CR
crPred :: Word8 -> Bool
crPred = (== 0x0d)
cr :: Parser Word8
cr = satisfy crPred <?> "carriage return"
{-# INLINABLE cr #-}

charPred w = w >= 0 || w <= 0x7f
-- | Parse a character
char :: Parser Word8
char = satisfy charPred
{-# INLINABLE char #-}

bitPred w = w == 48 || w == 49
-- | Parse a Bit
bit :: Parser Word8
bit = satisfy bitPred
{-# INLINABLE bit #-}

alphaPred w = (w >= 0x41 && w <= 0x5a) || (w >= 0x61 && w <= 0x7a)
-- | Parse an alpha
alpha :: Parser Word8
alpha = satisfy alphaPred
{-# INLINABLE alpha #-}

-- | Match a parser at least @N@ times.
manyN :: Int -> Parser a -> Parser [a]
manyN n p
    | n <= 0    = return []
    | otherwise = (++) <$> count n p <*> many' p
{-# INLINABLE manyN #-}

-- | Match a parser at least @N@ times, but no more than @M@ times.
manyNtoM :: Int -> Int -> Parser a -> Parser [a]
manyNtoM n m p
    | n <  0    = return []
    | n >  m    = return []
    | n == m    = count n p
    | n == 0    = foldr (<|>) (return []) (map (\x -> try $ count x p) (Prelude.reverse [1..m]))
    | otherwise = (++) <$> count n p <*> manyNtoM 0 (m - n) p
{-# INLINABLE manyNtoM #-}
