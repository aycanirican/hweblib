{-# LANGUAGE OverloadedStrings #-}

{-  parsing emails according to Rfc 5322 and friends... -}

module Network.Message
  ( parseMessage
  , MessageParseError(..)
  , ContentType (..)
  , Message (..)
  , MediaType
  , SubType
  , Parameters
  , contentType
  , defaultContentType
  , contentLength
  , lookupParameter
  , multiPartBody
  , message
  , body
  , subject
  , from
  , to
  , attachment
    -- * utils
  , removeAngles
  , mkAttachment
  , attachments
  , attachmentMessage
  , attachmentName
  , attachmentBody
  , decodeMessage
  , flattenParts
  , findParts
  )
  where

--------------------------------------------------------------------------------
import           Data.Maybe (fromMaybe)
import           Data.Either
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString as BS
import           Data.ByteString.Char8 as BSC
import qualified Network.Parser.Rfc5322 as R5322
import           Network.Parser.Rfc5322 ( Message (..) )
import qualified Network.Parser.Rfc2045 as R2045
import           Network.Parser.Rfc2183 as R2183
import qualified Network.Parser.Rfc2046 as R2046
import           Data.Semigroup ((<>))
import qualified Codec.MIME.Base64 as Base64
--------------------------------------------------------------------------------
-- * Types

type RawMessage       = ByteString
type RawMessageBody   = ByteString
type RawBoundaryValue = ByteString
type ParseResult = Either MessageParseError Message

data MessageParseError
  = CannotParse String                -- general parse error
  | UnsupportedContentType String
    deriving (Show)
             
cannotParse :: String -> ParseResult
cannotParse = Left . CannotParse

-- * Parsing

-- | There are two useful parsers given in this section. `message` is
-- a raw Rfc5322 parser which parses headers and the raw body section
-- of the Rfc 5322 message and returns `Message` data type.
--
-- The second parser `parseMessage` gets a Rfc5322 message and tries
-- to extract parts of it if it is a multipart message.

parse5322Message :: R5322.Message -> ParseResult
parse5322Message msg5322
  = case body msg5322 of
      Nothing      -> Right msg5322  -- empty body
      Just rawbody -> 
        case contentType msg5322 of  -- non-empty message body, possible parts
          Just (ContentType "multipart" _ ps) ->
            case lookupParameter "boundary" ps of
              Nothing       -> Right msg5322
              Just rawbndry ->
                case multiPartBody rawbndry rawbody of
                  Left  _  -> Right msg5322   -- cannot parse multipart body
                  Right ms ->
                    Right msg5322 { messageBody  = Nothing -- We ignore preamble here
                                  , messageParts = rights $ Prelude.map parse5322Message ms
                                  }
          Just ContentType{} -> Right msg5322 -- not a multipart message
          Nothing            -> Right msg5322 -- no content type

-- | First parse Rfc5322 message and then try to parse mime parts
parseMessage :: RawMessage -> ParseResult
parseMessage = either (cannotParse . ("Unable to parse raw Rfc5322 message: " <>)) parse5322Message . message
  
-- | We describe content-type header as a data type which has a
-- media-type, a subtype and parameter list (here we use associated
-- lists to represent them). You can use `lookupParameter` to query
-- parameters.

type MediaType  = ByteString
type SubType    = ByteString
type Parameters = [(ByteString, ByteString)]

data ContentType
  = ContentType MediaType SubType Parameters
    deriving Show

defaultContentType :: ContentType
defaultContentType = ContentType "text" "plain" [("charset", "utf8")]

-- | Parse a `ByteString` into a Rfc5322 `Message`
message :: ByteString -> Either String Message
message = parseOnly R5322.message

-- | Body of the message
body :: Message -> Maybe ByteString
body = R5322.messageBody

-- | Subject of the message
subject :: Message -> Maybe ByteString
subject = R5322.subjectHeader

from :: Message -> Maybe ByteString
from = R5322.fromHeader

to :: Message -> Maybe ByteString
to = R5322.toHeader

-- | Extract content-type header
contentType :: Message -> Maybe ContentType
contentType msg = case R5322.contentTypeHeader msg of
  Nothing -> Nothing
  Just v  -> case parseOnly parseContentType v of
    Left  _ -> Nothing
    Right r -> Just r
  where
    parseContentType :: Parser ContentType
    parseContentType = ContentType <$> (R2045.mtype <* word8 47)
                                   <*> (BS.pack <$> R2045.token)
                                   <*> many' (R2045.semicolonsp *> R2045.parameter)

-- | Extract content-length header as an `Int`
contentLength :: Message -> Maybe Int
contentLength m = case R5322.contentLengthHeader m of
    Nothing -> Nothing
    Just v  -> case parseOnly parseContentLength v of
      Left  _ -> Nothing
      Right r -> Just r
  where
    parseContentLength :: Parser Int
    parseContentLength = read <$> many1 AC.digit

-- wrap it since we may want to change implementation...
lookupParameter :: ByteString -> [(ByteString, ByteString)] -> Maybe ByteString
lookupParameter = Prelude.lookup

-- | Given a boundary string and message body, extract mime parts from the body part
multiPartBody :: RawBoundaryValue -> RawMessageBody -> Either String [Message]
multiPartBody boundary = parseOnly (R2046.multipartBody boundary)

type Body       = ByteString
type Name       = ByteString
type Attachment = (Message, Name, Maybe Body)

mkAttachment :: Message -> Name -> Maybe Body -> Attachment
mkAttachment msg name bdy = (msg, name, bdy)

attachmentName :: Attachment -> ByteString
attachmentName (_, n,_) = n

attachmentBody :: Attachment -> Maybe ByteString
attachmentBody (_,_,b) = b

attachmentMessage :: Attachment -> Message
attachmentMessage (m,_,_) = m

-- | Given a message, it tries to extract the attachment

attachment :: Message -> Either String Attachment
attachment msg
  = case R5322.contentDispositionHeader msg of
      Nothing -> Left "content-disposition header not found"
      Just hv -> case parseOnly dispositionParser hv of
        Left   err -> Left $ "Unable to parse disposition header: " ++ err
        Right disp -> case dispositionType disp of
          R2183.Attachment -> case L.find matchFilename (dispositionParameters disp) of
            Just (R2183.Filename name) -> case R5322.messageBody msg of
              Nothing -> Right $ mkAttachment msg name (Just "")
              Just _  -> Right $ mkAttachment msg name (Just (decodeMessage msg))
            _       -> Left "attachment doesn't have a filename"
          _         -> Left "Message disposition is not an attachment"
  where
    matchFilename :: DispositionParameter -> Bool
    matchFilename (Filename _) = True
    matchFilename _            = False

decodeMessage :: Message -> Body
decodeMessage msg
  = let bdy = fromMaybe "" (messageBody msg)
    in case R5322.contentTransferEncodingHeader msg of
      Just "base64"           -> BS.pack . Base64.decode . BSC.unpack $ bdy
      Just "quoted-printable" -> R2045.quotedPrintable bdy -- TODO: inefficient
      _                       -> bdy
       
attachments :: Message -> [Attachment]
attachments = Data.Either.rights . fmap attachment . messageParts

-- * Utilities

-- | Given a nameaddress bytestring, it removes angles and returns
-- rest of it as `Text`
removeAngles :: ByteString -> T.Text
removeAngles = modify . T.decodeUtf8
  where modify = T.takeWhile (/='>') . T.dropWhile (=='<') . T.dropWhile (==' ')

-- | Flatten nested parts into a list

-- Should flatten one root and two children
-- >>> :set -XOverloadedStrings
-- >>> flattenParts (Message [] (Just "root") [Message [] (Just "fst") [], Message [] (Just "snd") []])
-- [Message {messageFields = [], messageBody = Just "root", messageParts = []},Message {messageFields = [], messageBody = Just "fst", messageParts = []},Message {messageFields = [], messageBody = Just "snd", messageParts = []}]

-- Should flatten two levels
-- >>> r2 = Message { messageFields = [], messageBody = Just "root-2", messageParts = [    ] }
-- >>> r1 = Message { messageFields = [], messageBody = Just "root-1", messageParts = [ r2 ] }
-- >>> root = Message { messageFields = [], messageBody = Just "root", messageParts = [ r1 ] }
-- >>> flattenParts root
-- [Message {messageFields = [], messageBody = Just "root", messageParts = []},Message {messageFields = [], messageBody = Just "root-1", messageParts = []},Message {messageFields = [], messageBody = Just "root-2", messageParts = []}]


flattenParts :: Message -> [Message]
flattenParts msg@(Message _ _ []) = [msg]
flattenParts (Message h b parts ) = (Message h b []): L.concatMap flattenParts parts

-- | Find parts whose matching the predicate `pred'`

-- >>> :set -XOverloadedStrings
-- >>> hdr1 = R5322.mkHeaderField "From" "iricanaycan@gmail.com"
-- >>> msg = (Message [] (Just "root") [Message [] (Just "fst") [], Message [hdr1] (Just "snd") []])
-- >>> p1 = (== (Just "snd")) . messageBody
-- >>> findParts p1 msg
-- [Message {messageFields = [("From","iricanaycan@gmail.com")], messageBody = Just "snd", messageParts = []}]
-- >>> p2 = (== []) . messageFields
-- >>> findParts p2 msg
-- [Message {messageFields = [], messageBody = Just "fst", messageParts = []}]
findParts :: (Message -> Bool) -> Message -> [Message]
findParts pred' msg = L.filter pred' (flattenParts msg)
