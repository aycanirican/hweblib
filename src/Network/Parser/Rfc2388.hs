{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.2388
-- Copyright   :  Aycan iRiCAN 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Returning Values from Forms: multipart/form-data
--
-- <http://www.ietf.org/rfc/rfc2388.txt>

module Network.Parser.Rfc2388 where
import Control.Applicative (Alternative (many, (<|>)))
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (char, stringCI)
import Data.Functor (($>))
import Network.Parser.Rfc2183
  ( Disposition (..),
    DispositionType (..),
    dispositionParamParser,
    dispositionTypeParser,
  )
import Network.Parser.Rfc2234 (lwsp)

-- | 3. Definition of multipart/form-data

-- >>> parseOnly disposition "Content-Disposition: form-data; name=\"user\""
-- Right (Disposition {dispType = DispFormData, dispParams = [OtherParam "name" "user"]})
disposition :: Parser Disposition
disposition
  = do _ <- stringCI "Content-Disposition:" >> lwsp
       (ty, xs) <- (,) <$> dispositionTypeParser <*> many (char ';' >> lwsp *> dispositionParamParser)
       return $ Disposition ty xs

dispositionType :: Parser DispositionType
dispositionType
  =   dispositionTypeParser
  <|> stringCI "form-data" $> FormData


