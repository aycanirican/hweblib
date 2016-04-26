{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.2388
-- Copyright   :  Aycan iRiCAN 2010-2015
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
import           Control.Applicative
import           Control.Monad                    (join)
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString
import qualified Data.ByteString.Char8            as C
import           Data.Map.Strict                  as M
import           Data.Word                        (Word8)
--------------------------------------------------------------------------------
import           Network.Parser.Mime
import qualified Network.Parser.Rfc2183           as R2183
import           Network.Parser.Rfc2234
import           Network.Parser.Rfc2822           (comment, msg_id, text)
import           Network.Parser.RfcCommon         hiding (text)
--------------------------------------------------------------------------------

-- | 3. Definition of multipart/form-data

-- >>> parseOnly disposition "Content-Disposition: form-data; name=\"user\""
-- Right (Disposition {dispType = DispFormData, dispParams = [OtherParam "name" "user"]})
disposition :: Parser Disposition
disposition
  = do AC.string "Content-Disposition:" >> lwsp
       (ty, xs) <- (,) <$> dispositionType <*> many (AC.char ';' >> lwsp *> R2183.dispositionParam)
       return $ Disposition ty xs

dispositionType :: Parser DispType
dispositionType
  =   R2183.dispositionType
  <|> AC.stringCI "form-data" *> return DispFormData


