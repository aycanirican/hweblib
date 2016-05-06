{-# LANGUAGE OverloadedStrings #-}

module Network.Parser.PermuteTest where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString
import           Network.Parser.Permute

import           Data.Functor

data MyData
  = MyData (Maybe Bool) (Maybe ByteString) Int
  deriving Show

parseString :: Parser ByteString
parseString = "utku" <|> "aycan" <|> "true"

parseBool :: Parser Bool
parseBool = ("true" $> True) <|> ("false" $> False)

parseInt :: Parser Int
parseInt = read . pure <$> digit

parseMyData :: Parser MyData
parseMyData = permute $ MyData <$?> (Nothing, Just <$> parseBool)
                               <|?> (Nothing, Just <$> parseString)
                               <||> parseInt
