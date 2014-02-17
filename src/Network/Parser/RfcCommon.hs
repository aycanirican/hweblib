{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | This module provides common parsers used in rfc specifications.

module Network.Parser.RfcCommon where
--------------------------------------------------------------------------------
import           Control.Applicative        hiding (many)
import           Control.Monad              (join)
import           Data.Attoparsec            as AW
import qualified Data.Attoparsec.ByteString as DAC
import           Data.Attoparsec.Char8      as AC hiding (char, digit)
import           Data.Attoparsec.Combinator
import           Data.ByteString            as W
import           Data.ByteString.Char8      as C
import           Data.Word
import           Prelude                    hiding (take, takeWhile)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2234
--------------------------------------------------------------------------------
----
-- | Common Parsers
----

-- | Parse at least one hex digit.
hex :: Parser [Word8]
hex = many1 hexdig
{-# INLINE hex #-}

-- | Parse lws and return space
lws :: Parser Word8
lws = (try (crlf *> s) <|> s) *> return 32 <?> "lightweight space"
  where s = many1 (sp <|> ht)
{-# INLINE lws #-}

-- | consecutive matches of lws rule, where they MUST be compressed to
-- a single 0x20 byte
lwss :: Parser Word8
lwss = return 32 <$> many' lws
{-# INLINE lwss #-}

-- | Parse a character but not a control or parenthesis
ctext :: Parser Word8
ctext = crlf <|> AW.satisfy char_not_ctl_or_paren
  where
    char_not_ctl_or_paren w
        = charPred w && not (w == 40 || w == 41) && not (ctlPred w)
{-# INLINE ctext #-}

-- | Parse a character but not a control character or double quote
qdtext :: Parser Word8
qdtext = crlf <|> AW.satisfy char_not_ctl_or_dquote
  where
    char_not_ctl_or_dquote w
        = charPred w && not (dquotePred w) && not (ctlPred w)
{-# INLINE qdtext #-}

-- | Parse quoted pair
quotedPair :: Parser [Word8]
quotedPair = ret <$> word8 92 <*> char
    where ret a b = [a,b]
{-# INLINE quotedPair #-}

-- | Parse quoted string
quotedString :: Parser [Word8]
quotedString = do
  r <- word8 34 *> manyTill (do
                             a <- option [] quotedPair
                             b <- qdtext
                             return $ a ++ [b])
                            (word8 34)
  return . join $ r
{-# INLINE quotedString #-}

-- | Parse a character but not a control character.
text :: Parser Word8
text = crlf <|> AW.satisfy char_not_ctl
  where char_not_ctl w = charPred w && not (ctlPred w)
{-# INLINE text #-}

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
