{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.RfcCommon
-- Copyright   :  Aycan iRiCAN 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- This module provides common parsers used in rfc specifications.

module Network.Parser.RfcCommon where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Functor (($>))
import           Data.Attoparsec.ByteString as BS
import           Data.Attoparsec.Combinator as BS
import           Data.Word
import           Debug.Trace (traceM)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2234
--------------------------------------------------------------------------------

-- | Simple debug function which prints info about read stream
debug :: Parser a -> Parser a
debug p = do
  (consumed, result) <- match p
  k <- lookAhead takeByteString
  traceM ("consumed: "  <> show consumed
       <> "remaining: " <> show k)
  return result

-- | Common Parsers
----

-- | Parse at least one hex digit.
hex :: Parser [Word8]
hex = many1 hexdig
{-# INLINABLE hex #-}

-- | Parse lws and return space
lws :: Parser Word8
lws = ((crlf *> s) <|> s) *> return 32 <?> "lightweight space"
  where s = many1 (sp <|> ht)
{-# INLINABLE lws #-}

-- | consecutive matches of lws rule, where they MUST be compressed to
-- a single 0x20 byte
lwss :: Parser Word8
lwss =  many' lws $> 32
{-# INLINABLE lwss #-}

-- | Parse a character but not a control or parenthesis
ctext :: Parser Word8
ctext = crlf <|> BS.satisfy char_not_ctl_or_paren
  where
    char_not_ctl_or_paren w
        = charPred w && not (w == 40 || w == 41) && not (ctlPred w)
{-# INLINABLE ctext #-}

-- | Parse a character but not a control character or double quote
qdtext :: Parser Word8
qdtext = crlf <|> BS.satisfy char_not_ctl_or_dquote
  where
    char_not_ctl_or_dquote w
        = charPred w && not (dquotePred w) && not (ctlPred w)
{-# INLINABLE qdtext #-}

-- | Parse quoted pair
quotedPair :: Parser [Word8]
quotedPair = ret <$> word8 92 <*> char
    where ret a b = [a,b]
{-# INLINABLE quotedPair #-}

-- | Parse a character but not a control character.
text :: Parser Word8
text = crlf <|> BS.satisfy char_not_ctl
  where char_not_ctl w = charPred w && not (ctlPred w)
{-# INLINABLE text #-}

--------------
-- | Utilities
--------------
asList :: Parser a -> Parser [a]
asList p = (:[]) <$> p
