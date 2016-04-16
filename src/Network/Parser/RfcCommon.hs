{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Module      :  Network.Parser.RfcCommon
-- Copyright   :  Aycan iRiCAN 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- This module provides common parsers used in rfc specifications.

module Network.Parser.RfcCommon where
--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Control.Monad              (join)
import           Data.Attoparsec.ByteString as BS
import           Data.ByteString            as W
import           Data.ByteString.Char8      as C
import           Data.Word
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2234
--------------------------------------------------------------------------------

-- | Common Parsers
----

-- | Parse at least one hex digit.
hex :: Parser [Word8]
hex = many1 hexdig
{-# INLINABLE hex #-}

-- | Parse lws and return space
lws :: Parser Word8
lws = (try (crlf *> s) <|> s) *> return 32 <?> "lightweight space"
  where s = many1 (sp <|> ht)
{-# INLINABLE lws #-}

-- | consecutive matches of lws rule, where they MUST be compressed to
-- a single 0x20 byte
lwss :: Parser Word8
lwss = return 32 <$> many' lws
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

-- | Parse quoted string
quotedString :: Parser [Word8]
quotedString = do
  r <- word8 34 *> manyTill (do
                             a <- option [] quotedPair
                             b <- qdtext
                             return $ a ++ [b])
                            (word8 34)
  return . join $ r
{-# INLINABLE quotedString #-}

-- | Parse a character but not a control character.
text :: Parser Word8
text = crlf <|> BS.satisfy char_not_ctl
  where char_not_ctl w = charPred w && not (ctlPred w)
{-# INLINABLE text #-}

--------------
-- | Utilities
--------------

appcon :: [a] -> [[a]] -> [a]
appcon = (. join) . (++)

-- | return a word inside a list
word8l w = (:[]) <$> word8 w

-- | Convert a ByteString Word to ByteString Char
toRepr = C.unpack . W.pack

asList :: Parser a -> Parser [a]
asList p = (:[]) <$> p
