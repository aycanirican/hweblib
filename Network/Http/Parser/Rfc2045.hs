{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  #-}

module Network.Http.Parser.Rfc2045 where

import Control.Monad (join)
import Control.Applicative as A hiding (many)
import Data.Attoparsec
import qualified Data.Attoparsec.Char8 as AC
import Data.Attoparsec.FastSet (fromList, memberWord8)
import Data.ByteString as W
import Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import Network.Http.Parser.RfcCommon
import Network.Http.Parser.Rfc2234


ietfToken :: Parser ByteString
ietfToken = A.empty

ianaToken :: Parser ByteString
ianaToken = A.empty

-- Prelude.map Data.Char.ord "()<>@,;:\\\"/[]?="
tspecialsSet ::[Word8]
tspecialsSet = [40,41,60,62,64,44,59,58,92,34,47,91,93,63,61]
tspecials_pred w = memberWord8 w (fromList tspecialsSet)
tspecials :: Parser Word8
tspecials = satisfy tspecials_pred

token_pred w = char_pred w && not (ctl_pred w || tspecials_pred w)
token :: Parser [Word8]
token = many1 $ satisfy token_pred

attribute = token
parameter = (,) <$> (attribute <* word8 61) <*> value

xToken_pred w = token_pred w && (w /= 32)
xToken :: Parser ByteString
xToken = AC.stringCI "x-" *> (W.pack <$> many1 (satisfy xToken_pred))

version :: Parser (Int,Int)
version = (,)
          <$> (AC.stringCI "mime-version" *> word8 58 *> lws *> AC.decimal) -- ':'
          <*> (word8 46 *> AC.decimal) -- '.'

value :: Parser [Word8]
value = token <|> quotedString

mtype = discreteType <|> compositeType
subtype = extensionToken <|> ianaToken

extensionToken = ietfToken <|> xToken

discreteType = AC.stringCI "text" <|> AC.stringCI "image"
               <|> AC.stringCI "audio" <|> AC.stringCI "video"
               <|> AC.stringCI "application" <|> extensionToken

compositeType = AC.stringCI "message" <|> AC.stringCI "multipart" <|> extensionToken

safeChar_pred w = (w >= 33 && w <= 60) || (w >= 62 && w <= 126)

safeChar :: Parser Word8
safeChar = satisfy safeChar_pred

hexOctet :: Parser Word8
hexOctet = ret <$> (word8 61 *> hexdig) <*> hexdig
    where ret a b = toTen a * 16 + toTen b
          toTen w | w >= 48 && w <= 57  =  fromIntegral (w - 48)
                  | w >= 97 && w <= 102 =  fromIntegral (w - 87)
                  | otherwise           =  fromIntegral (w - 55)

transportPadding = option [32] lwsp

ptext = hexOctet <|> safeChar

qpSection = many (ptext <|> sp <|> ht)

qpSegment :: Parser [Word8]
qpSegment = ret <$> qpSection <*> many (sp <|> ht) <*> word8 61
    where ret a [] c = a
          ret a b  c = a ++ [32]

qpPart = qpSection

qpLine = do
  a <- many $ (++) <$> qpSegment <*> (transportPadding <* crlf)
  b <- (++) <$> qpPart <*> transportPadding
  
  return $ join a ++ b

quotedPrintable :: Parser ByteString
quotedPrintable = do 
  a <- appcon <$> qpLine <*> many (crlf *> qpLine)
  return $ W.pack a