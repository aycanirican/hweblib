{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.2183
-- Copyright   :  Aycan iRiCAN 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Communicating Presentation Information in Internet Messages:
--     The Content-Disposition Header Field
-- <http://www.ietf.org/rfc/rfc2183.txt>

module Network.Parser.Rfc2183
  where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString                  as W
import qualified Data.Map                         as M
import           Data.Monoid
import           Data.Scientific
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as E
import           Data.Time
import           Data.Word                        (Word8)
import           Prelude                          hiding (take, takeWhile)
import           Prelude                          hiding (id)
--------------------------------------------------------------------------------
import           Network.Parser.Mime
import           Network.Parser.Rfc2045
import           Network.Parser.Rfc2234
import           Network.Parser.Rfc2822
import           Network.Parser.RfcCommon         hiding (text)
--------------------------------------------------------------------------------

-- >>> parseOnly disposition "Content-Disposition: attachment; filename=genome.jpeg;\n  modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\""
-- Right (Disposition {dispType = DispAttachment, dispParams = [Filename "genome.jpeg",ModDate 1997-02-12 21:29:51 UTC]})
disposition :: Parser Disposition
disposition
  = do AC.string "Content-Disposition:" >> lwsp
       ty <- dispositionType
       xs <- many (AC.char ';' >> lwsp  *> dispositionParam)
       return $ Disposition ty xs

dispositionType :: Parser DispType
dispositionType
  =   AC.stringCI "inline"     *> return DispInline
  <|> AC.stringCI "attachment" *> return DispAttachment
  <|> (extensionToken         >>= return . DispOther . E.decodeLatin1)

-- >>> parseOnly dispositionParam "filename=genome.jpeg"
-- Right (Filename "genome.jpeg")
-- >>> parseOnly dispositionParam "modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\""
-- Right (ModDate 1997-02-12 21:29:51 UTC)
dispositionParam :: Parser DispParam
dispositionParam
  = filenameParm <|> creationDateParm <|> modificationDateParm <|> readDateParm <|> myParameter
  where
    myParameter :: Parser DispParam
    myParameter = do
      (k,v) <- parameter
      return $ OtherParam (E.decodeLatin1 k) (E.decodeLatin1 v)

-- >>> parseOnly filenameParm "filename=foobar.jpeg"
filenameParm :: Parser DispParam
filenameParm = Filename . E.decodeLatin1 <$> (AC.string "filename=" *> value)

-- >>> parseOnly (mkDateParam "read-date") "read-date=\"Mon, 21 Sep 1980 10:01:02 +0230\""
-- Right 1980-09-21 07:31:02 UTC
mkDateParam :: ByteString -> Parser UTCTime
mkDateParam key = AC.string (key <> "=") *> quotedDateTime

creationDateParm, modificationDateParm, readDateParm :: Parser DispParam
creationDateParm     = CreationDate <$> mkDateParam "creation-date"
modificationDateParm = ModDate      <$> mkDateParam "modification-date"
readDateParm         = ReadDate     <$> mkDateParam "read-date"

-- >>> parseOnly sizeParam "size=-44242"
-- Left "Failed reading: size cannot be negative"
-- >>> parseOnly sizeParam "size=-44242.2"
-- Left "Failed reading: size is not an integer"
sizeParam :: Parser DispParam
sizeParam = do
  x <- AC.string "size=" *> AC.scientific
  case floatingOrInteger x of
    Left  f -> fail "size is not an integer"
    Right i -> if 0 > i then fail "size cannot be negative" else return (Size i)

quotedDateTime :: Parser UTCTime
quotedDateTime = AC.char '"' *> dateTime <* AC.char '"'

