{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  #-}

-- http://www.ietf.org/rfc/rfc2045.txt
--
-- Multipurpose Internet Mail Extensions (MIME) Part One: Format of
-- Internet Message Bodies

module Network.Http.Parser.Rfc2045 where

import Control.Monad (join)
import Control.Applicative as A hiding (many)
import Data.Attoparsec
import qualified Data.Attoparsec.Char8 as AC
import qualified Data.Attoparsec.FastSet as F (fromList, memberWord8)
import Data.ByteString as W
import Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import Network.Http.Parser.RfcCommon
import Network.Http.Parser.Rfc2234
import Network.Http.Parser.Rfc2822 (msg_id)
import qualified Data.Map as M
import Prelude hiding (id)

-- * 4.  MIME-Version Header Field

version :: Parser (Int,Int)
version = (,)
          <$> (AC.stringCI "mime-version" *> colonsp *> AC.decimal) -- ':'
          <*> (word8 46 *> AC.decimal) -- '.'

-- * 5. Content-Type Header Field

ietfToken :: Parser ByteString
ietfToken = A.empty

ianaToken :: Parser ByteString
ianaToken = A.empty

-- Prelude.map Data.Char.ord "()<>@,;:\\\"/[]?="
tspecialsSet ::[Word8]
tspecialsSet = [40,41,60,62,64,44,59,58,92,34,47,91,93,63,61]
tspecials_pred :: Word8 -> Bool
tspecials_pred w = F.memberWord8 w (F.fromList tspecialsSet)
tspecials :: Parser Word8
tspecials = satisfy tspecials_pred

token_pred :: Word8 -> Bool
token_pred w = char_pred w && not (w == 32 || ctl_pred w || tspecials_pred w)
token :: Parser [Word8]
token = many1 $ satisfy token_pred

attribute :: Parser [Word8]
attribute = token

parameter :: Parser (ByteString,ByteString)
parameter = res <$> (attribute <* word8 61) <*> value
    where res a v = (W.pack a, W.pack v)

xToken_pred :: Word8 -> Bool
xToken_pred w = token_pred w && (w /= 32)
xToken :: Parser ByteString
xToken = AC.stringCI "x-" *> (W.pack <$> many1 (satisfy xToken_pred))

value :: Parser [Word8]
value = token <|> quotedString

mtype :: Parser (Either Discrete Composite)
mtype =  eitherP discreteType compositeType

-- TODO: subtype = extensionToken <|> ianaToken
subtype :: Parser ByteString
subtype = W.pack <$> token

-- TODO: extensionToken = xToken <|> ietfToken 
extensionToken :: Parser ByteString
extensionToken = xToken

discreteType :: Parser Discrete
discreteType = (Text <$ AC.stringCI "text")
               <|> (Image <$ AC.stringCI "image")
               <|> (Audio <$ AC.stringCI "audio")
               <|> (Video <$ AC.stringCI "video")
               <|> (Application <$ AC.stringCI "application")
               <|> DiscreteExtension <$> extensionToken

compositeType :: Parser Composite
compositeType = (Message <$ AC.stringCI "message")
                <|> (MultiPart <$ AC.stringCI "multipart")
                <|> CompositeExtension <$> extensionToken

content :: Parser MimeContent
content = res <$> (AC.stringCI "content-type" *> colonsp *> mtype)
          <*> (word8 47 *> subtype)
          <*> many (semicolonsp *> parameter)
    where res a b ps = MimeContent a b (M.fromList ps)

-- * 6. Content-Transfer-Encoding Header Type

encoding :: Parser ByteString
encoding = AC.stringCI "content-transfer-encoding" *> colonsp *> mechanism

mechanism :: Parser ByteString
mechanism = AC.stringCI "7bit" 
            <|> AC.stringCI "8bit"
            <|> AC.stringCI "binary" 
            <|> AC.stringCI "quoted-printable" 
            <|> AC.stringCI "base64" 
            <|> xToken <|> ietfToken

-- * Quoted Printable

safeChar_pred :: Word8 -> Bool
safeChar_pred w = (w >= 33 && w <= 60) || (w >= 62 && w <= 126)

safeChar :: Parser Word8
safeChar = satisfy safeChar_pred

hexOctet :: Parser Word8
hexOctet = ret <$> (word8 61 *> hexdig) <*> hexdig
    where ret a b = toTen a * 16 + toTen b
          toTen w | w >= 48 && w <= 57  =  fromIntegral (w - 48)
                  | w >= 97 && w <= 102 =  fromIntegral (w - 87)
                  | otherwise           =  fromIntegral (w - 55)

transportPadding :: Parser [Word8]
transportPadding = option [32] lwsp

ptext :: Parser Word8
ptext = hexOctet <|> safeChar

qpSection :: Parser [Word8]
qpSection = many (ptext <|> sp <|> ht)

qpSegment :: Parser [Word8]
qpSegment = ret <$> qpSection <*> many (sp <|> ht) <*> word8 61
    where ret a [] _ = a
          ret a _  _ = a ++ [32]

qpPart :: Parser [Word8]
qpPart = qpSection

qpLine :: Parser [Word8]
qpLine = do
  a <- many $ (++) <$> qpSegment <*> (transportPadding <* crlf)
  b <- (++) <$> qpPart <*> transportPadding
  
  return $ join a ++ b

quotedPrintable :: Parser ByteString
quotedPrintable = do 
  a <- appcon <$> qpLine <*> many (crlf *> qpLine)
  return $ W.pack a

-- * 7.  Content-ID Header Field
contentId = W.pack <$> (AC.stringCI "content-id" *> colonsp *> msg_id)

-- * 8.  Content-Description Header Field
-- TODO: support 2047 encoding
description = AC.stringCI "content-description" *> colonsp *> many text

-- * 9. Additional MIME Header Fields
-- TODO: support 822 header fields
mimeExtensionField = AC.stringCI "content-" *> token *> colonsp *> many text


-- entityHeaders = do
--   c <- try (content <* crlf)
--   e <- try (encoding <* crlf)
--   i <- try (id <* crlf)
--   d <- try (description <* crlf)
--   rest <- many (mimeExtensionField <* crlf)
--   return [c,e,i,d] ++ rest


-- * Utilities
colonsp :: Parser ()
colonsp = word8 58 *> lws *> pure ()

semicolonsp :: Parser ()
semicolonsp = word8 59 *> lws *> pure ()

-- * ADTs

data HeaderField 
    = HeaderField 
      { hName :: ByteString
      , hValue :: ByteString
      , hParams :: M.Map ByteString ByteString
      }

data Discrete
    = Text 
    | Image 
    | Audio
    | Video
    | Application
    | DiscreteExtension ByteString 
      deriving (Eq, Show)

data Composite
    = Message
    | MultiPart
    | CompositeExtension ByteString 
      deriving (Eq, Show)

data MimeContent
    = MimeContent 
      { mcType :: Either Discrete Composite
      , mcSubtype :: ByteString
      , mcParams :: M.Map ByteString ByteString
      }
