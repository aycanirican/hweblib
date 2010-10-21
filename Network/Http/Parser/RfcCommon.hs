{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  , DeriveDataTypeable
  , TupleSections
  #-}

module Network.Http.Parser.RfcCommon where

import Control.Applicative hiding (many)
import Data.Attoparsec as AW
import Data.Attoparsec.Combinator as DACO
import Data.Attoparsec.Char8 as AC hiding (digit, char)
import qualified Data.Attoparsec.Char8 as DAC
import qualified Data.Attoparsec.FastSet as AF
import Data.ByteString as W
import Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8, Word64)
import Data.Char (digitToInt, isAsciiUpper, isAsciiLower)
import Control.Monad (join)
import Prelude hiding (take, takeWhile)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Network.Http.Parser.Rfc2234

----
-- * Common Parsers
----

hex :: Parser [Word8]
hex = many1 hexdig
{-# INLINE hex #-}

-- parse lws and return space
lws :: Parser Word8
lws = (try (crlf *> s) <|> s) *> return 32 <?> "lightweight space"
  where s = many1 (sp <|> ht)
{-# INLINE lws #-}

-- consecutive matches of lws rule, where they MUST be compressed to a
-- single 0x20 byte
lwss :: Parser Word8
lwss = return 32 <$> many lws
{-# INLINE lwss #-}

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
{-# INLINE qdtext #-}

quotedPair :: Parser [Word8]
quotedPair = ret <$> word8 92 <*> char
    where ret a b = a:b:[]
{-# INLINE quotedPair #-}

quotedString :: Parser [Word8]
quotedString = do
  word8 34 
  r <- many (do a <- option [] quotedPair 
                b <- qdtext
                return $ a ++ [b]
            )
  word8 34
  return . join $ r
{-# INLINE quotedString #-}

text :: Parser Word8
text = crlf <|> AW.satisfy char_not_ctl
  where char_not_ctl w = char_pred w && not (ctl_pred w)
{-# INLINE text #-}

-- | Utilities
appcon :: [a] -> [[a]] -> [a]
appcon = (. join) . (++)
word8l w = (:[]) <$> word8 w
toRepr = C.unpack . W.pack

asList :: Parser a -> Parser [a]
asList p = (:[]) <$> p
