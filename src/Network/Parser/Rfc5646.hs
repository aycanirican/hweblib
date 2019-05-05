{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.5646
-- Copyright   :  Aycan iRiCAN 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Tags for Identifying Languages
--
-- <http://www.ietf.org/rfc/rfc5646.txt>

module Network.Parser.Rfc5646 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString                  as B hiding (count, singleton)
import           Data.Word
--------------------------------------------------------------------------------
import           Network.Parser.Rfc5234
--------------------------------------------------------------------------------
-- * 2.1.  Syntax

-- >>> parseOnly language_tag "de"
-- Right "de"
-- >>> parseOnly language_tag "zh-Hans"
-- Right "zh-Hans"
-- >>> parseOnly language_tag "x-whatever"
-- Right "x-whatever"
language_tag :: Parser ByteString
language_tag = langtag <|> privateuse <|> grandfather

langtag :: Parser ByteString
langtag
  = ret <$> language
        <*> option mempty (B.cons 45 <$> (A.word8 45 *> script))
        <*> option mempty (B.cons 45 <$> (A.word8 45 *> region))
        <*> many (B.cons 45 <$> (A.word8 45 *> variant))
        <*> many (B.cons 45 <$> (A.word8 45 *> extension))
        <*> option mempty (B.cons 45 <$> (A.word8 45 *> privateuse))
  where ret a b c ds es f = a <> b <> c <> (B.concat ds) <> (B.concat es) <> f

language :: Parser ByteString
language = (<>) <$> (pack <$> manyNtoM 2 3 alpha)
                <*> option mempty ((B.cons 45) <$> (A.word8 45 *> extlang))

{-
  Original definition was:

  extlang       = 3ALPHA              ; selected ISO 639 codes
                  *2("-" 3ALPHA)      ; permanently reserved

  Implemented like this due to overlapping parsers:

  extlang       = 3ALPHA               ; selected ISO 639 codes
                  1*2("-" 3ALPHA)      ; permanently reserved

-}
extlang :: Parser ByteString
extlang
  = ret <$> count 3 alpha
        <*> (join . fmap (45:)
             <$> (manyNtoM 1 2 (A.word8 45 *> count 3 alpha)))
  where ret xs ys = pack (xs ++ ys)

script :: Parser ByteString
script = pack <$> count 4 alpha

region :: Parser ByteString
region = pack <$> (count 2 alpha <|> count 3 digit)

variant :: Parser ByteString
variant
  = pack <$> (manyNtoM 5 8 alphanum <|> ((:) <$> digit <*> count 3 alphanum))

extension :: Parser ByteString
extension
  = ret <$> singleton
        <*> many1 (A.word8 45 *> manyNtoM 2 8 alphanum)
  where
    ret s sx = pack (s:(join . fmap (45:) $ sx))

singleton :: Parser Word8
singleton
  = digit
    <|> satisfy (\w -> (w >= 0x41 && w <= 0x57)
                       || ( w >= 0x59 && w <= 0x5a)
                       || (w >= 0x61 && w <= 0x77)
                       || (w >= 0x79 && w <= 0x7a))

privateuse :: Parser ByteString
privateuse
  = AC.char 'x' *> (pack . (120:) . join . fmap (45:)
                    <$> many1 (A.word8 45 *> manyNtoM 1 8 alphanum))

grandfather :: Parser ByteString
grandfather = irregular <|> regular

irregular :: Parser ByteString
irregular
  = AC.string "en-GB-oed"
    <|> AC.string "i-ami"
    <|> AC.string "i-bnn"
    <|> AC.string "i-default"
    <|> AC.string "i-enochian"
    <|> AC.string "i-hak"
    <|> AC.string "i-klingon"
    <|> AC.string "i-lux"
    <|> AC.string "i-mingo"
    <|> AC.string "i-navajo"
    <|> AC.string "i-pwn"
    <|> AC.string "i-tao"
    <|> AC.string "i-tay"
    <|> AC.string "i-tsu"
    <|> AC.string "sgn-BE-FR"
    <|> AC.string "sgn-BE-NL"
    <|> AC.string "sgn-CH-DE"

regular :: Parser ByteString
regular
  = AC.string "art-lojban"
    <|> AC.string "cel-gaulish"
    <|> AC.string "no-bok"
    <|> AC.string "no-nyn"
    <|> AC.string "zh-guoyu"
    <|> AC.string "zh-hakka"
    <|> AC.string "zh-min"
    <|> AC.string "zh-min-nan"
    <|> AC.string "zh-xiang"

alphanum :: Parser Word8
alphanum = alpha <|> digit
