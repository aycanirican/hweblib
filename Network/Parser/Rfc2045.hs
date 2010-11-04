{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  #-}

-- http://www.ietf.org/rfc/rfc2045.txt
--
-- Multipurpose Internet Mail Extensions (MIME) Part One: 
--   Format of Internet Message Bodies

module Network.Parser.Rfc2045 where

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
import Network.Parser.RfcCommon
import Network.Parser.Rfc2234
import Network.Parser.Rfc2822 (msg_id, comment)
import qualified Data.Map as M

-- TODO: implement fields of rfc 822

-- TODO: we don't support comments on header values "MIME-Version:
-- 1.(produced by MetaSend Vx.x)0" which we should parse and ignore.


-- * 3. MIME Header Fields

--mimePartHeaders = ret <$> entityHeaders <*> option [] fields
--  where ret eh f = ...
--mimeMessageHeaders = ret <$> entityHeaders <*> fields <*> (version <* crlf)
--  where ret eh f v = ...

entityHeaders :: Parser [Header]
entityHeaders = many1 entityHeader

entityHeader :: Parser Header
entityHeader = version <* crlf
               <|> content <* crlf
               <|> encoding <* crlf
               <|> description <* crlf
               <|> contentId <* crlf
               <|> mimeExtensionField <* crlf

-- * 4.  MIME-Version Header Field

version :: Parser Header
version = ret <$> (AC.stringCI "mime-version" *> colonsp *> AC.decimal) -- ':'
          <*> (word8 46 *> AC.decimal) -- '.'
    where ret a b = Header VersionH (W.pack [a+48,46,b+48]) M.empty

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

parameter :: Parser (ByteString, ByteString)
parameter = res <$> (attribute <* word8 61) <*> value
    where res a v = (W.pack a, W.pack v)

xToken_pred :: Word8 -> Bool
xToken_pred w = token_pred w && (w /= 32)
xToken :: Parser ByteString
xToken = AC.stringCI "x-" *> (W.pack <$> many1 (satisfy xToken_pred))

value :: Parser [Word8]
value = token <|> quotedString

-- discrete and composite types are a little bit different from BNF
-- definitions in order to keep code flow clean.
mtype :: Parser ByteString
mtype = AC.stringCI "multipart"
        <|> AC.stringCI "text"
        <|> AC.stringCI "image"
        <|> AC.stringCI "audio"
        <|> AC.stringCI "video"
        <|> AC.stringCI "application"
        <|> AC.stringCI "message"
        <|> extensionToken

-- TODO: subtype = extensionToken <|> ianaToken
subtype :: Parser ByteString
subtype = W.pack <$> token

-- TODO: extensionToken = xToken <|> ietfToken 
extensionToken :: Parser ByteString
extensionToken = xToken

content :: Parser Header
content = res <$> (AC.stringCI "content-type" *> colonsp *> mtype)
          <*> (word8 47 *> subtype)
          <*> many (semicolonsp *> parameter)
    where res a b ps = Header ContentH (C.concat [a, "/", b]) (M.fromList ps)

-- * 6. Content-Transfer-Encoding Header Type

encoding :: Parser Header
encoding = ret <$> (AC.stringCI "content-transfer-encoding" *> colonsp *> mechanism)
    where ret m = Header EncodingH m M.empty

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
contentId :: Parser Header
contentId = ret <$> (AC.stringCI "content-id" *> colonsp *> msg_id)
    where ret i = Header IdH (W.pack i) M.empty

-- * 8.  Content-Description Header Field
-- TODO: support 2047 encoding
description :: Parser Header
description = ret <$> (AC.stringCI "content-description" *> colonsp *> many text)
    where ret d = Header DescriptionH (W.pack d) M.empty

-- * 9. Additional MIME Header Fields
-- TODO: support 822 header fields
mimeExtensionField :: Parser Header
mimeExtensionField = do
  k <- AC.stringCI "content-" *> token
  v <- colonsp *> many text
  return $ Header (ExtensionH $ W.pack k)  (W.pack v) M.empty

-- * Utilities
colonsp :: Parser ()
colonsp = word8 58 *> lws *> pure ()

semicolonsp :: Parser ()
semicolonsp = word8 59 *> lws *> pure ()

-- | * Temporary ADTs
---------------------
data HeaderType
    = ContentH
    | EncodingH
    | IdH
    | DescriptionH
    | VersionH
    | ExtensionH ByteString
      deriving (Eq,Show,Ord)

data Header
    = Header 
      { hType :: HeaderType
      , hValue :: ByteString
      , hParams :: M.Map ByteString ByteString
      } deriving (Eq, Show)

