{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.2183
-- Copyright   :  Aycan iRiCAN 2010-2020
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
  ( Disposition (..)
  , DispositionType (..)
  , DispositionParameter (..)
  , dispositionParser
  , dispositionTypeParser
  , dispositionParamParser
  , sizeParamParser
  , quotedDateTimeParser
  ) where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as E
import           Data.Time
import           Data.Semigroup                   ((<>))
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2045           (extensionToken, parameter,
                                                   value)
import           Network.Parser.Rfc2234           (lwsp)
import           Network.Parser.Rfc2822           (dateTime)
--------------------------------------------------------------------------------

-- | A Disposition has a type and list of parameters
data Disposition
    = Disposition
      { dispositionType       :: DispositionType
      , dispositionParameters :: [DispositionParameter]
      } deriving (Eq, Show)

data DispositionType
    = Inline
    | Attachment
    | FormData
    | OtherDisposition T.Text
      deriving (Eq, Show)

-- TODO: Convert it to a value level map
data DispositionParameter
    = Name ByteString
    | Filename ByteString
    | CreationDate UTCTime
    | ModDate UTCTime
    | ReadDate UTCTime
    | Size Integer
    | OtherParameter T.Text T.Text
      deriving (Eq, Show)

-- >>> :set -XOverloadedStrings
-- >>> parseOnly dispositionParser " attachment; filename=genome.jpeg;\n  modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\""
-- Right (Disposition {dispositionType = Attachment, dispositionParameters = [Filename "genome.jpeg",ModDate 1997-02-12 21:29:51 UTC]})
dispositionParser :: Parser Disposition
dispositionParser
  = Disposition <$> (lwsp *> dispositionTypeParser)
                <*> many (";" *> lwsp  *> dispositionParamParser)
    
-- | Note: implementation of multipart/form-data is defined in Rfc2388
dispositionTypeParser :: Parser DispositionType
dispositionTypeParser
  =   AC.stringCI "inline"     *> return Inline
  <|> AC.stringCI "attachment" *> return Attachment
  <|> (OtherDisposition . E.decodeLatin1 <$> extensionToken)

-- >>> :set -XOverloadedStrings
-- >>> parseOnly dispositionParamParser "filename=genome.jpeg"
-- Right (Filename "genome.jpeg")
-- >>> parseOnly dispositionParamParser "modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\""
-- Right (ModDate 1997-02-12 21:29:51 UTC)
dispositionParamParser :: Parser DispositionParameter
dispositionParamParser
  = nameParm <|> filenameParm <|> creationDateParm <|> modificationDateParm <|> readDateParm <|> myParameter
  where
    myParameter :: Parser DispositionParameter
    myParameter = do
      (k,v) <- parameter
      return $ OtherParameter (E.decodeLatin1 k) (E.decodeLatin1 v)

-- >>> :set -XOverloadedStrings
-- >>> parseOnly nameParm "name=\"this is my name!\""
-- Right (Name "this is my name!")
nameParm :: Parser DispositionParameter
nameParm = Name <$> (AC.stringCI "name=" *> value)

-- >>> parseOnly filenameParm "filename=foobar.jpeg"
-- Right (Filename "foobar.jpeg")
filenameParm :: Parser DispositionParameter
filenameParm = Filename <$> (AC.stringCI "filename=" *> value)

-- >>> parseOnly (mkDateParam "read-date") "read-date=\"Mon, 21 Sep 1980 10:01:02 +0230\""
-- Right 1980-09-21 07:31:02 UTC
mkDateParam :: ByteString -> Parser UTCTime
mkDateParam key = AC.stringCI (key <> "=") *> quotedDateTimeParser

creationDateParm, modificationDateParm, readDateParm :: Parser DispositionParameter
creationDateParm     = CreationDate <$> mkDateParam "creation-date"
modificationDateParm = ModDate      <$> mkDateParam "modification-date"
readDateParm         = ReadDate     <$> mkDateParam "read-date"

-- >>> parseOnly sizeParamParser "size=44242"
-- Right (Size 44242)
-- >>> parseOnly sizeParamParser "size=-44242.2"
-- Left "Only digits allowed > digit: Failed reading: satisfyWith"
sizeParamParser :: Parser DispositionParameter
sizeParamParser = Size . read <$> (AC.stringCI "size=" *> (many1 AC.digit <?> "Only digits allowed") )

-- >>> parseOnly quotedDateTimeParser "\"Wed, 12 Feb 1997 16:29:51 -0500\""
-- Right 1997-02-12 21:29:51 UTC
quotedDateTimeParser :: Parser UTCTime
quotedDateTimeParser = AC.char '"' *> dateTime <* AC.char '"'

