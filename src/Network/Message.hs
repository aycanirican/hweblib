{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

{-  parsing emails according to Rfc 5322 and friends... -}

module Network.Message
  ( parseMessage
  , ParsedMessage
  , mkParsedMessage
  , topLevelMessage
  , partsOfMessage
  , UnableToParse
  , ContentType (..)
  , Message
  , MediaType
  , SubType
  , Parameters
  , contentType
  , contentLength
  , lookupParameter
  , multiPartBody
  , message
  , body
  , subject
  , attachment
    -- * utils
  , removeAngles
  , mkAttachment
  , attachments
  , attachmentName
  , attachmentBody
  )
  where

--------------------------------------------------------------------------------
import Data.Either
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString as BS
import qualified Network.Parser.Rfc5322 as R5322
import qualified Network.Parser.Rfc2045 as R2045
import           Network.Parser.Rfc2183 as R2183
import qualified Network.Parser.Rfc2046 as R2046
import Data.Semigroup ((<>))
--------------------------------------------------------------------------------
-- * Types

-- | A rfc5322 message
type Message = R5322.Message -- messageFields, messageBody

-- | Parsed message consists of the main message and it's mime parts
-- also represented as list of messages
type ParsedMessage = (Message, [Message])
mkParsedMessage :: Message -> [Message] -> ParsedMessage
mkParsedMessage = (,)

topLevelMessage :: ParsedMessage -> Message
topLevelMessage = fst

partsOfMessage :: ParsedMessage -> [Message]
partsOfMessage = snd

-- | UnableToParse includes parse error as a string and a partially
-- filled Message record, which may or may not include all of the
-- parts of the original raw message
type UnableToParse = (String, Maybe Message)


-- * Parsing

-- | Parse a message as deep as we can.
parseMessage
  :: BS.ByteString
  -> Either UnableToParse ParsedMessage
parseMessage rawMessage
  = case message rawMessage of
      Left  err -> Left ("Unable to parse raw message: " <> err, Nothing)
      Right msg -> case body msg of
        Nothing   -> Right (msg, []) 
        Just msgbody -> case contentType msg of -- try to determine contenttype
          Just (ContentType "multipart" "mixed" ps) ->
            case lookupParameter "boundary" ps of
              Nothing    -> Left ("cannot find boundary in content-type header for multipart message", Just msg) -- TODO: handle no boundary case
              Just bndry -> case multiPartBody bndry msgbody of
                Left err    -> Left (err, Just msg)
                Right parts -> Right (msg, parts) -- ParsedMessage
          Just _ -> case contentLength msg of
              Nothing -> Left ("no content length detected!", Just msg)
              Just l  -> Left ("content-length found: " <> show l, Just msg)
          Nothing -> Right (msg, [])

-- * Useful data types and functions goes here..

-- | We describe content-type header as a data type which has a
-- media-type, a subtype and parameter list (here we use associated
-- lists to represent them). You can use `lookupParameter` to query
-- parameters.
type MediaType = ByteString
type SubType   = ByteString
type Parameters = [(ByteString, ByteString)]

data ContentType = ContentType MediaType SubType Parameters

-- | Parse a Message from given bytestring
message :: ByteString -> Either String Message
message = parseOnly R5322.message

-- | Body of the message
body :: Message -> Maybe ByteString
body = R5322.messageBody

-- | Subject of the message
subject :: Message -> Maybe ByteString
subject = R5322.subjectHeader

-- | Extract content-type header
contentType :: Message -> Maybe ContentType
contentType msg = case R5322.contentTypeHeader msg of
  Nothing -> Nothing
  Just v  -> case parseOnly parseContentType v of
    Left  l -> Nothing
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
      Left  l -> Nothing
      Right r -> Just r
  where
    parseContentLength :: Parser Int
    parseContentLength = read <$> many1 AC.digit

-- wrap it since we may want to change implementation...
lookupParameter :: ByteString -> [(ByteString, ByteString)] -> Maybe ByteString
lookupParameter = Prelude.lookup

-- | Given a boundary string, extract mime parts from message body
multiPartBody :: ByteString -> ByteString -> Either String [Message]
multiPartBody boundary = parseOnly (R2046.multipartBody boundary)


type Body       = ByteString
type Name       = ByteString
type Attachment = (Name, Maybe Body)

mkAttachment :: Name -> Maybe Body -> Attachment
mkAttachment name bdy = (name, bdy)

attachmentName :: Attachment -> ByteString
attachmentName = fst

attachmentBody :: Attachment -> Maybe ByteString
attachmentBody = snd

-- | Given a message, it tries to extract the attachment
attachment :: Message -> Either String Attachment
attachment msg
  = case R5322.contentDispositionHeader msg of
      Nothing -> Left "content-disposition header not found"
      Just hv -> case parseOnly dispositionParser hv of
        Left   err -> Left "Unable to parse disposition header"
        Right disp -> case dispositionType disp of
          R2183.Attachment -> case L.find matchFilename (dispositionParameters disp) of
            Just (R2183.Filename name) -> Right $ mkAttachment name     (R5322.messageBody msg)
            _                          -> Left    "attachment doesn't have a filename"
          _          -> Left "Message disposition is not an attachment"
  where
    matchFilename :: DispositionParameter -> Bool
    matchFilename (Filename _) = True
    matchFilename _            = False

attachments :: ParsedMessage -> [Attachment]
attachments = Data.Either.rights . fmap attachment . partsOfMessage

-- * Utilities

-- | Given a nameaddress bytestring, it removes angles and returns
-- rest of it as `Text`
removeAngles :: ByteString -> T.Text
removeAngles = modify . T.decodeUtf8
  where modify = T.takeWhile (/='>') . T.dropWhile (=='<') . T.dropWhile (==' ')

-- >>> :set -XOverloadedStrings
-- >>> let msg = R5322.Message [R5322.mkHeaderField "content-disposition" "attachment; filename=\"foo.html\""] (Just "contentz")
-- >>> attachment msg
-- Right ("foo.html",Just "contentz")
-- >>> let msgQuotedFilename = R5322.Message [R5322.mkHeaderField "content-disposition" "attachment; filename=\"foo.html\""] (Just "contentz")
-- >>> attachment msg
-- Right ("foo.html",Just "contentz")

-- pPrintNoColor . either error attachment.  

-- >>> :set -XOverloadedStrings
-- >>> import Text.Pretty.Simple (pPrint, pPrintNoColor)
-- >>> msg <- Data.ByteString.Char8.readFile "/tmp/foo-attachment"
-- >>> either show attachment $ parseMessage $ msg
-- <interactive>:144:14-23: error:
--     • Couldn't match type ‘Either String Network.Message.Attachment’
--                      with ‘[Char]’
--       Expected type: Message -> String
--         Actual type: Message -> Either String Network.Message.Attachment
--     • In the second argument of ‘either’, namely ‘attachment’
--       In the expression: either show attachment
--       In the expression: either show attachment $ parseMessage $ msg
-- <interactive>:144:27-44: error:
--     • Couldn't match type ‘(Message, [Message])’
--                      with ‘Network.Parser.Rfc5322.Message’
--       Expected type: Either UnableToParse Message
--         Actual type: Either UnableToParse ParsedMessage
--     • In the second argument of ‘($)’, namely ‘parseMessage $ msg’
--       In the expression: either show attachment $ parseMessage $ msg
--       In an equation for ‘it’:
--           it = either show attachment $ parseMessage $ msg
