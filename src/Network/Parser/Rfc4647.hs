{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Module      :  Network.Parser.4647
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Matching of Language Tags
--
-- <http://www.ietf.org/rfc/rfc4647.txt>

module Network.Parser.Rfc4647 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Attoparsec.ByteString       as BS
import           Data.Attoparsec.ByteString.Char8 as AC hiding (digit)
import           Data.Attoparsec.Combinator
import           Data.ByteString
import           Data.Char                        (digitToInt)
import           Data.Monoid
import           Data.Scientific
import           Data.Time
import           Data.Typeable
import           Data.Word
import qualified GHC.Generics                     as GHC
--------------------------------------------------------------------------------
import           Network.Parser.Rfc5234
--------------------------------------------------------------------------------

-- * 2.1.  Basic Language Range

{-
   language-range   = (1*8ALPHA *("-" 1*8alphanum)) / "*"
   alphanum         = ALPHA / DIGIT
-}
language_range :: Parser ByteString
language_range
  = (ret <$> aterm
     <*> many ((45:) <$> (AC.char '-' *> anterm)))
    <|> (AC.char '*' *> return (pack [42]))
  where
    aterm = manyNtoM 1 8 alpha
    anterm = manyNtoM 1 8 alphanum
    ret x xs = pack (x ++ (Prelude.concat xs))

alphanum :: Parser Word8
alphanum = alpha <|> digit

-- * 2.2.  Extended Language Range
{- TODO
   extended-language-range = (1*8ALPHA / "*")
                             *("-" (1*8alphanum / "*"))
-}
