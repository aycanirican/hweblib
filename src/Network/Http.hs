{-# LANGUAGE OverloadedStrings #-}

module Network.Http
  where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (replicateM)
import Data.Attoparsec.ByteString
  ( Parser,
    anyWord8,
    many',
    word8,
  )
import Data.Attoparsec.ByteString.Char8 as AC
  ( string,
  )
import Data.ByteString (ByteString, pack)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Network.Parser.Rfc2045 as R2045
import qualified Network.Parser.Rfc2046 as R2046
import qualified Network.Parser.Rfc5234 as R5234
import qualified Network.Parser.Rfc5322 as R5322
import qualified Network.Parser.Rfc7230 as R7230
import qualified Network.Parser.Rfc7231 as R7231

-- contentType :: Request -> Maybe ByteString
-- contentType = ehContentType . entityHeaders . rqHeaders

-- contentLength :: Request -> Maybe Int
-- contentLength = ehContentLength . entityHeaders . rqHreaders

-- parseMessage :: Parser R7230.HTTPMessage
-- parseMessage = do
--   sl <- R7230.start_line
--   xs <- many headerParser
--   where
--     headerParser = representationHeaderField
--     representationHeaderField = content_type
--                                 <|> content_encoding
--                                 <|> content_language
--                                 <|> content_location
--     payloadSemantics = content_length
--                        <|> content_range
--                        <|> trailer
--                        <|> transfer_encoding
--     controls = cache_control
--                <|> expect
--                <|> host
--                <|> max_forwards
--                <|> pragma
--                <|> range
--                <|> te
--     conditionals = if_match
--                    <|> if_none_match
--                    <|> if_modified_since
--                    <|> if_unmodified_since
--                    <|> if_range
--     contentNegotiation = accept
--                          <|> accept_charset
--                          <|> accept_encoding
--                          <|> accept_language
--     authCredentials = undefined

--     requestContext = from
--                      <|> referrer
--                      <|> user_agent
--     controlData = age
--                   <|> cache_control
--                   <|> expires
--                   <|> date
--                   <|> location
--                   <|> retry_after
--                   <|> vary
--                   <|> warning
--     validators = etag
--                  <|> last_modified

--     authentication_challenges = undefined

--     response_context = accept_ranges
--                        <|> allow
--                        <|> server

--     headers = [ ("Content-Length", R7230.content_length)
--               ]

data HTTPHeaders
  = HTTPHeaders { contentLength :: Maybe Integer
                , contentType   :: Maybe R7231.MediaType
                , otherHeaders  :: [(ByteString, ByteString)]
                }
  deriving (Show, Eq)

emptyHTTPHeaders :: HTTPHeaders
emptyHTTPHeaders = HTTPHeaders Nothing Nothing []

data Entity = Discrete ByteString
            | Composite [R5322.Message]
            deriving (Show, Eq)

parseMessage :: Parser (R7230.StartLine, HTTPHeaders, Maybe Entity)
parseMessage = do
  sl <- R7230.startLine
  fs <- many' $ (\i h -> h { contentLength=Just i })
                  <$> header "Content-Length" R7230.contentLength
            <|> (\i h -> h { contentType=Just i })
                  <$> header "Content-Type" R7231.contentType
            <|> (\i h -> h { otherHeaders = i : otherHeaders h } )
                  <$> R7230.headerField <* R5234.crlf
  let hs = foldl' (flip ($)) emptyHTTPHeaders fs
  _ <- R5234.crlf

  body <- case contentType hs of
    Just (R7231.MediaType "multipart" subtype xs) -> case "boundary" `lookup` xs of
      Just b  -> Just . Composite <$> case subtype of
        "mixed"       -> R2046.multipartBody b
        "alternative" -> R2046.multipartBody b
        "form-data"   -> R2046.multipartBody b
        _             -> fail $ "unknown content type: " ++ show subtype
      Nothing -> fail "boundary not found"
    _ -> case contentLength hs of
      Just l  -> Just . Discrete . pack <$> replicateM (fromInteger l) anyWord8
      Nothing -> return Nothing

  return (sl, hs, body)
  where
    header :: ByteString -> Parser a -> Parser a
    header h p = AC.string h *> word8 58 *> R7230.ows *> p <* R5234.crlf

-- TODO: case-insensitive lookup
hasHeader :: R7230.HTTPMessage -> ByteString -> Maybe ByteString
hasHeader rq key = lookup key (R7230.headers rq)

mimes :: Parser [R5322.Message]
mimes = do
  ehdrs <- R2045.entityHeaders <* R5234.crlf
  case cType ehdrs of
    Nothing       -> fail "No content-type defined in entity headers"
    Just (ty, hs) ->
      if ty `elem` ["multipart/mixed", "multipart/alternative"]
      then case M.lookup "boundary" hs of
             Nothing -> pure []
             Just b  -> R2046.multipartBody b
      else case M.lookup "boundary" hs of
             Nothing -> pure []
             Just _  -> R7230.asList R2046.bodyPart
  where
    cType :: [R2045.Header] -> Maybe (ByteString, M.Map ByteString ByteString)
    cType headers
      = let chs = Prelude.filter ((== R2045.ContentH) . R2045.hType) headers
        in case chs of
        []  -> Nothing
        a:_ -> Just (R2045.hValue a, R2045.hParams a)


{-
Utku says:

* Maybe Type's should be exported per RFC instead of Network.Types
* Some simple types can have pretty printers (maybe just for debugging purposes, like "URI", "StatusLine" etc)
* querying uriQuery and uriFragment, cookie etc
-}
