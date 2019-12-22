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
--------------------------------------------------------------------------------
import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char, stringCI)
----------------------------------------------------------------------
import qualified Network.Parser.Rfc2183 as R2183 ( dispositionParamParser
                                                 , dispositionTypeParser
                                                 , Disposition (..), DispositionType (..))
import Network.Parser.Rfc2234 hiding (char)
--------------------------------------------------------------------------------

-- | 3. Definition of multipart/form-data

-- >>> parseOnly disposition "Content-Disposition: form-data; name=\"user\""
-- Right (Disposition {dispType = DispFormData, dispParams = [OtherParam "name" "user"]})
disposition :: Parser R2183.Disposition
disposition
  = do _ <- stringCI "Content-Disposition:" >> lwsp
       (ty, xs) <- (,) <$> R2183.dispositionTypeParser <*> many (char ';' >> lwsp *> R2183.dispositionParamParser)
       return $ R2183.Disposition ty xs

dispositionType :: Parser R2183.DispositionType
dispositionType
  =   R2183.dispositionTypeParser
  <|> stringCI "form-data" *> return R2183.FormData


