{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.4647
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2020
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

import Control.Applicative (Alternative (many, (<|>)))
import Data.Attoparsec.ByteString as BS (Parser)
import Data.Attoparsec.ByteString.Char8 as AC (char)
import Data.ByteString (ByteString, pack)
import Data.Word (Word8)
import Data.Functor (($>))
import Network.Parser.Rfc5234 (alpha, digit, manyNtoM)

-- * 2.1.  Basic Language Range

{-
   language-range   = (1*8ALPHA *("-" 1*8alphanum)) / "*"
   alphanum         = ALPHA / DIGIT
-}
languageRange :: Parser ByteString
languageRange
    = (ret <$> aterm
           <*> many ((45:) <$> (AC.char '-' *> anterm)))
  <|> (AC.char '*' $> pack [42])
  where
    aterm    = manyNtoM 1 8 alpha
    anterm   = manyNtoM 1 8 alphaNum
    ret x xs = pack (x ++ Prelude.concat xs)

alphaNum :: Parser Word8
alphaNum = alpha <|> digit

-- * 2.2.  Extended Language Range
{- TODO
   extended-language-range = (1*8ALPHA / "*")
                             *("-" (1*8alphanum / "*"))
-}
