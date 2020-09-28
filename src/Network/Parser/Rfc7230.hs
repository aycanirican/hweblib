{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-- |
-- Module      :  Network.Parser.7230
-- Copyright   :  Aycan iRiCAN 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing
-- <http://www.ietf.org/rfc/rfc7230.txt>

module Network.Parser.Rfc7230
  where

import Control.Applicative (optional, Alternative (many, (<|>)))
import Control.Monad (join)
import Data.Attoparsec.ByteString
  ( Parser,
    count,
    inClass,
    many1,
    manyTill,
    option,
    parseOnly,
    satisfy,
    word8,
  )
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString (ByteString, pack, singleton)
import qualified Data.ByteString.Char8 as BSC
import Data.Scientific (Scientific)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Network.Parser.Rfc3986 as R3986
import Network.Parser.Rfc5234 as R5234
  ( alpha,
    crlf,
    digit,
    dquote,
    hexdig,
    htab,
    manyN,
    manyNtoM,
    octet,
    sp,
    vchar,
  )
import Network.Parser.Utils (maxP)
import Network.Types
  ( HTTPVersion (HTTPVersion),
    URI,
    URIAuth,
  )
import Numeric (readHex)
import Data.Functor (($>))

data StartLine
  = RequestLine ByteString RequestTarget HTTPVersion
  | StatusLine HTTPVersion ByteString ByteString
    deriving (Eq, Show, Generic, Typeable)

data HTTPMessage
  = HTTPMessage { status  :: StartLine
                , headers :: [(ByteString, ByteString)]
                , body    :: Maybe ByteString
                } deriving (Eq, Show, Generic, Typeable)

data TransferCoding
  = Chunked
  | Compress
  | Deflate
  | GZip
  | TransferExtension ByteString [(ByteString, ByteString)]
    deriving (Eq, Show, Generic, Typeable)

data Chunk
  = Chunk Int [(ByteString, ByteString)] ByteString
    deriving (Eq, Show, Generic, Typeable)

newtype TrailerPart = TrailerPart [(ByteString, ByteString)]
    deriving (Eq, Show, Generic, Typeable)

data ChunkedBody
  = ChunkedBody [Chunk] TrailerPart
    deriving (Eq, Show, Generic, Typeable)

data TCoding
  = TCoding TransferCoding (Maybe Scientific)
  | Trailers
    deriving (Eq, Show, Generic, Typeable)

data RequestTarget
  = OriginForm ByteString ByteString
  | AbsoluteForm URI
  | AuthorityForm (Maybe URIAuth)
  | AsteriskForm
    deriving (Eq, Show, Generic, Typeable)

data Protocol
  = ProtocolName ByteString
  | ProtocolVersion ByteString
  | ProtocolNameVersion ByteString ByteString deriving (Eq, Show, Generic, Typeable)

newtype Via = Via [(Protocol, ReceivedBy)] deriving (Eq, Show, Generic, Typeable)

data ReceivedBy
  = ReceivedByHost ByteString (Maybe ByteString) -- ^ hostname port
  | ReceivedByPseudonym ByteString       -- ^ token
    deriving (Eq, Show, Generic, Typeable)

-- >>> parse httpVersion "HTTP/1.1"
-- Done "" (HTTPVersion {httpMajor = 1, httpMinor = 1})
httpVersion :: Parser HTTPVersion
httpVersion = (AC.stringCI "http/1.0" $> HTTPVersion 1 0)
          <|> (AC.stringCI "http/1.1" $> HTTPVersion 1 1)

httpMessage :: Parser HTTPMessage
httpMessage = do
  sl <- startLine
  xs <- many (headerField <* crlf) <* crlf
  return $ HTTPMessage sl xs mempty

-- 2.7.  Uniform Resource Identifiers
{-
     absolute-path = 1*( "/" segment )
     partial-URI   = relative-part [ "?" query ]

-}
absolutePath :: Parser [ByteString]
absolutePath = fmap pack <$> many1 (AC.char '/' *> R3986.segment)

-- 3.1.  Start Line
startLine :: Parser StartLine
startLine = requestLine <|> statusLine

-- 3.1.1.  Request Line
requestLine :: Parser StartLine
requestLine = RequestLine <$> method <* sp <*> requestTarget <* sp <*> httpVersion <* crlf

method :: Parser ByteString
method = token

-- 3.1.2.  Status Line
statusLine :: Parser StartLine
statusLine = StatusLine <$> (httpVersion <* sp) <*> statusCode <* sp <*> reasonPhrase <* crlf

statusCode :: Parser ByteString
statusCode = pack <$> manyN 3 digit

reasonPhrase :: Parser ByteString
reasonPhrase = pack <$> many (htab <|> sp <|> vchar <|> obsText)

-- 3.2.  Header Fields
-- |
-- >>> parseOnly headerField "User-Agent: curl/7.47.0"
-- Right ("User-Agent","curl/7.47.0")
headerField :: Parser (ByteString, ByteString)
headerField = (,) <$> (fieldName <* word8 58 <* ows) <*> fieldValue <* ows

fieldName :: Parser ByteString
fieldName = token

fieldValue :: Parser ByteString
fieldValue = BSC.concat <$> many (fieldContent <|> obsFold)

-- | Slightly modified version to catch below example.
-- >>> parseOnly fieldContent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:29.0) Gecko/20100101 Firefox/29.0"
-- Right "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:29.0) Gecko/20100101 Firefox/29.0"
fieldContent :: Parser ByteString
fieldContent = do
  x  <- fieldVchar
  xs <- option mempty (many (sp <|> htab <|> fieldVchar))
  return $ pack (x:xs)

fieldVchar :: Parser Word8
fieldVchar = vchar <|> obsText

obsFold :: Parser ByteString
obsFold = ret <$> asList (ows *> crlf) <*> many1 (sp <|> htab)
  where
    ret x y = pack (x ++ y)

-- 3.2.3.  Whitespace
ows, rws, bws :: Parser [Word8]
ows = many (sp <|> htab)
rws = many1 (sp <|> htab)
bws = ows

-- 3.2.5.  Field Limits
token :: Parser ByteString
token = pack <$> many1 tchar

tchar :: Parser Word8
tchar = satisfy p <|> digit <|> alpha
  where p = inClass "!#$%&'*+.^_`|~-"

quotedString :: Parser ByteString
quotedString = pack . join <$> (dquote *> many (asList qdtext <|> quotedPair) <* dquote)

qdtext :: Parser Word8
qdtext = htab <|> sp <|> satisfy p <|> obsText
  where
    p w = w == 0x21
          || (w >= 0x23 && w <= 0x5b)
          || (w >= 0x5d && w <= 0x7e)

obsText :: Parser Word8
obsText = satisfy (\w -> w >= 0x80 && w <= 0xff)

comment :: Parser ByteString
comment = ret <$> (   word8 40
                      *> many (fmap singleton ctext
                               <|> fmap pack quotedPair
                               <|> comment)
                      <* word8 41)
  where ret x = "(" <> BSC.concat x <> ")"

ctext :: Parser Word8
ctext = htab <|> sp <|> satisfy p <|> obsText
  where p w = (w >= 0x21 && w <= 0x27)
              || (w >= 0x2a && w <= 0x5b)
              || (w >= 0x5d && w <= 0x7e)

quotedPair :: Parser [Word8]
quotedPair = (\a b -> [a,b]) <$> word8 92 <*> (htab <|> sp <|> vchar <|> obsText)

-- 3.3.  Message Body
messageBody :: Parser ByteString
messageBody = pack <$> many octet

-- 3.3.1.  Transfer-Encoding
transferEncoding :: Parser [TransferCoding]
transferEncoding = dash1 transferCoding

-- 3.3.2.  Content-Length
contentLength :: Parser Integer
contentLength = read <$> many1 AC.digit

-- 4.  Transfer Codings
transferCoding :: Parser TransferCoding
transferCoding = (AC.stringCI "chunked"  $> Chunked)
             <|> (AC.stringCI "compress" $> Compress)
             <|> (AC.stringCI "deflate"  $> Deflate)
             <|> (AC.stringCI "gzip"     $> GZip)
             <|> transferExtension

transferExtension :: Parser TransferCoding
transferExtension
  = TransferExtension <$> token
                      <*> many (ows *> AC.char ';' *> ows *> transferParameter)

transferParameter :: Parser (ByteString, ByteString)
transferParameter = (,) <$> (token <* bws <* AC.char '=') <*> (bws *> (token <|> quotedString))

-- 4.1.  Chunked Transfer Coding

chunkedBody :: Parser ChunkedBody
chunkedBody = ChunkedBody <$> manyTill chunk lastChunk <*> (trailerPart <* crlf)

chunk :: Parser Chunk
chunk = do
  n   <- chunkSize
  ext <- option [] chunkExt <* crlf
  d   <- count n octet <* crlf
  return $ Chunk n ext (pack d)

chunkSize :: Parser Int
chunkSize = do
  a <- readHex . BSC.unpack . pack <$> many1 hexdig
  case a of
    []      -> fail "invalid chunk size"
    [(i,_)] -> return i
    _       -> fail "invalid chunk size"

lastChunk :: Parser Chunk
lastChunk = lastChunk' <$> (many1 (AC.char '0') *> option [] chunkExt <* crlf)
  where
    lastChunk' ext = Chunk 0 ext mempty

chunkData :: Parser ByteString
chunkData = pack <$> many1 octet

-- 4.1.1.  Chunk Extensions
chunkExt :: Parser [(ByteString, ByteString)]
chunkExt = many ((,) <$> (AC.char ';' *> ows *> chunkExtName)
                      <*> option mempty (AC.char '=' *> chunkExtVal))

chunkExtName :: Parser ByteString
chunkExtName = token

chunkExtVal :: Parser ByteString
chunkExtVal = token <|> quotedString

-- 4.1.2.  Chunked Trailer Part
trailerPart :: Parser TrailerPart
trailerPart = TrailerPart <$> many (headerField <* crlf)

-- 4.3.  TE
te :: Parser [TCoding]
te = dash tCodings

tCodings :: Parser TCoding
tCodings = (Trailers <$ AC.stringCI "trailers")
       <|> (TCoding <$> transferCoding <*> optional tRanking)

tRanking :: Parser Scientific
tRanking = ows *> AC.char ';' *> ows *> AC.stringCI "q=" *> rank

rank :: Parser Scientific
rank = (AC.char '0' *> option 0 prec)
   <|> (AC.char '1' *> option 1 (AC.char '.' *> manyNtoM 0 3 (AC.char '0') $> 1))
  where
    prec :: Parser Scientific
    prec = do
      x <- parseOnly AC.scientific . pack . ([48,46] ++) <$> (AC.char '.'  *> manyNtoM 0 3 digit)
      case x of
        Left  str -> fail str
        Right sci -> return sci

-- 4.4.  Trailer
trailer :: Parser [ByteString]
trailer = dash1 fieldName

-- 5.3.  Request Target
requestTarget :: Parser RequestTarget
requestTarget = absoluteForm
            <|> (AsteriskForm <$ asteriskForm)
            <|> originForm
            <|> authorityForm

-- 5.3.1.  origin-form
originForm :: Parser RequestTarget
originForm = OriginForm <$> R3986.pathAbsolute <*> option mempty (AC.char '?' *> R3986.query)

-- 5.3.2.  absolute-form
absoluteForm :: Parser RequestTarget
absoluteForm = AbsoluteForm <$> R3986.absoluteUri

-- 5.3.3.  authority-form
authorityForm :: Parser RequestTarget
authorityForm = AuthorityForm <$> R3986.authority

-- 5.3.4.  asterisk-form
asteriskForm :: Parser RequestTarget
asteriskForm = AC.char '*' >> return AsteriskForm

-- 5.4.  Host
host :: Parser (ByteString, Maybe ByteString)
host = (,) <$> uriHost <*> optional (AC.char ':' *> port)

port :: Parser ByteString
port = R3986.port

uriHost :: Parser ByteString
uriHost = R3986.host

-- 5.7.  Message Forwarding

-- 5.7.1.  Via
via :: Parser Via
via = Via <$> dash1 ((,) <$> (receivedProtocol <* rws)
                         <*> receivedBy <* option mempty (rws <* comment))

receivedProtocol :: Parser Protocol
receivedProtocol = do 
  n <- option Nothing (fmap Just (protocolName <* AC.char '/'))
  v <- protocolVersion
  return $ case n of
    Just n' -> ProtocolNameVersion n' v
    Nothing -> ProtocolVersion v

receivedBy :: Parser ReceivedBy
receivedBy = (uncurry ReceivedByHost <$> host)
         <|> (ReceivedByPseudonym    <$> pseudonym)

pseudonym :: Parser ByteString
pseudonym = token

-- 6.  Connection Management
-- 6.1.  Connection
connection :: Parser [ByteString]
connection = dash1 connectionOption

connectionOption :: Parser ByteString
connectionOption = token

-- 6.7.  Upgrade
upgrade :: Parser [Protocol]
upgrade = dash1 protocol

protocol :: Parser Protocol
protocol = do n <- protocolName
              v <- optional (AC.char '/' *> protocolVersion)
              return $ case v of
                Just v' -> ProtocolNameVersion n v'
                Nothing -> ProtocolName n

protocolName :: Parser ByteString
protocolName = token

protocolVersion :: Parser ByteString
protocolVersion = token

-- 7.  ABNF List Extension: #rule
{- A #rule extension to the ABNF rules of [RFC5234] is used to improve
   readability in the definitions of some header field values.

   A construct "#" is defined, similar to "*", for defining
   comma-delimited lists of elements.  The full form is "<n>#<m>element"
   indicating at least <n> and at most <m> elements, each separated by a
   single comma (",") and optional whitespace (OWS). -}

dash :: Parser a -> Parser [a]
dash p = (:) <$> p <*> many (ows *> word8 44 *> ows *> p)

-- |
-- >>> parseOnly (dash1 AC.digit) "1"
-- Right "1"
-- >>> parseOnly (dash1 AC.digit) "1,2"
-- Right "12"
-- >>> parseOnly (dash1 token) ""
-- Left "not enough input"
dash1 :: Parser a -> Parser [a]
dash1 = dashN 1

dashN :: Int -> Parser a -> Parser [a]
dashN 0 _ = return []
dashN n p
  | n == 0    = return []
  | otherwise = (\a b c -> a:(b++c)) <$> p
                                     <*> count (n-1) (addSep p)
                                     <*> many        (addSep p)
  where 
    addSep x = ows *> word8 44 *> ows *> x

dashNtoM :: Int -> Int -> Parser a -> Parser [a]
dashNtoM n m p
  | n < 0     = return []
  | n > m     = return []
  | n == m    = (:) <$> p <*> count (n-1) (sep *> p)
  | n == 0    = sepByMaxN m p sep
  | otherwise = (++) <$> sepByN n p sep <*> (sep *> dashNtoM 0 (m-n) p)
  where
    sep = ows *> word8 44 *> ows $> ()

-- | Parse `n` times separated by `s`
sepByN :: Int -> Parser a -> Parser () -> Parser [a]
sepByN 0 _ _ = return mempty
sepByN n p s = (:) <$> p <*> count (n-1) (s *> p)

sepByMaxN :: Int -> Parser a -> Parser () -> Parser [a]
sepByMaxN 0 _ _ = return mempty
sepByMaxN n p s = ((:) <$> p <*> maxP (n-1) (s *> p)) <|> return mempty

-- dashNtoM :: Parser a -> Parser [a]
-- dashNtoM n m p
--   | n < 0  = return []
--   | n > m  = return []
--   | n == m = count n p
--   | n == 0 = fold (<|>) (return []) (map (\x -> count x p) (Prelude.reverse [1..m]))
--   | otherwise = (++) count n p <*> dashNtoM 0 (m-n) p

-- -- | * Basic Parser Constructs for RFC 2616
-- separatorsPred, tokenPred :: Word8 -> Bool
-- tokenPred w = charPred w && not (ctlPred w || separatorsPred w)

-- -- Page: 16
-- -- token = 1*<any CHAR except CTLs or separators>
-- token :: Parser [Word8]
-- token = many1 $ satisfy tokenPred
-- {-# INLINABLE token #-}

-- -- "()<>@,;:\\\"/[]?={} \t"
-- -- separatorSet :: [Word8]
-- -- separatorSet = [40,41,60,62,64,44,59,58,92,34,47,91,93,63,61,123,125,32,9]
-- separatorsPred = inClass "()<>@,;:\\\"/[]?={} \t"
-- -- Page: 16
-- -- separators     = "(" | ")" | "<" | ">" | "@"
-- --                | "," | ";" | ":" | "\" | <">
-- --                | "/" | "[" | "]" | "?" | "="
-- --                | "{" | "}" | SP | HT
-- separators :: Parser Word8
-- separators = satisfy separatorsPred
-- {-# INLINABLE separators #-}

-- comment :: Parser [Word8]
-- comment
--   = word8 40
--     *> (Prelude.concat <$> many' (quotedPair <|> ((:[]) <$> ctext)))
--     <* word8 41

-- -- parse (httpVersion) (pack "HTTP/12.15\n")
-- httpVersion :: Parser HTTPVersion
-- httpVersion
--   =   stringCI "http/1.0" *> return (HTTPVersion 1 0)
--   <|> stringCI "http/1.1" *> return (HTTPVersion 1 1)
--   <|> stringCI "http/2.0" *> return (HTTPVersion 2 0)

-- -- parse (method) (W.pack "GET /")
-- method :: Parser Method
-- method = (GET         <$ stringCI "get")
--          <|> (PUT     <$ stringCI "put")
--          <|> (POST    <$ stringCI "post")
--          <|> (HEAD    <$ stringCI "head")
--          <|> (DELETE  <$ stringCI "delete")
--          <|> (TRACE   <$ stringCI "trace")
--          <|> (CONNECT <$ stringCI "connect")
--          <|> (OPTIONS <$ stringCI "options")
--          <|> ((EXTENSIONMETHOD . pack) <$> token)

-- requestUri :: Parser RequestUri
-- requestUri = try (Asterisk <$ word8 42)
--              <|> RelativeRef <$> R3986.relativeRef
--              <|> AbsoluteUri <$> R3986.absoluteUri
--              <|> (AbsolutePath . pack) <$> R3986.pathAbsolute
--              <|> Authority <$> R3986.authority

-- -- parse requestLine (C.pack "GET /my.cgi?foo=bar&john=doe HTTP/1.1\n")
-- requestLine :: Parser (Method, RequestUri, HTTPVersion)
-- requestLine = ret <$> method      <* sp
--                   <*> requestUri  <* sp
--                   <*> httpVersion <* crlf
--     where ret m u h = (m,u,h)

-- headerContentNcPred :: Word8 -> Bool
-- headerContentNcPred w
--        = (w >= 0x00 && w <= 0x08)
--       || (w >= 0x0b && w <= 0x0c)
--       || (w >= 0x0e && w <= 0x1f)
--       || (w >= 0x21 && w <= 0x39)
--       || (w >= 0x3b && w <= 0xff)

-- headerContent :: Parser Word8
-- headerContent = satisfy (\w -> headerContentNcPred w || w == 58) -- ':'

-- -- >>> parseOnly headerName "Content-Length: 1024"
-- -- Right "Content-Length"
-- headerName :: Parser ByteString
-- headerName = pack <$> (many1 . satisfy $ headerContentNcPred)

-- -- >>> parseOnly headerValue "This is a multi\n  line value."
-- -- Right "This is a multi line value."
-- headerValue :: Parser ByteString
-- headerValue = do
--   c <- headerContent
--   r <- option [] (many' (headerContent <|> lws)) -- TODO: http://stuff.gsnedders.com/http-parsing.txt
--   return . pack $ (c:r)

-- -- >>> parseOnly header "Content-Type: text/html;\n  charset=ISO-8859-9"
-- -- Right ("Content-Type","text/html; charset=ISO-8859-9")
-- header :: Parser (ByteString,ByteString)
-- header = (,) <$> headerName  <* (word8 58 <* lwss)
--              <*> headerValue <* lwss

-- entityBody :: Parser [Word8]
-- entityBody = many' octet

-- messageBody :: Parser [Word8]
-- messageBody = entityBody

-- requestHeaders :: Parser Headers
-- requestHeaders = do
--   (m, ru, v) <- requestLine <?> "request line"
--   many (    httpGeneralHeader
--         <|> httpRequestHeader
--         <|> httpEntityHeader
--         <|> httpUnknownHeader)
--   return undefined
--   where
--     header :: ByteString -> Parser ByteString
--     header key = stringCI key *> word8 58 *> lwss *> headerValue <* lwss

--     httpGeneralHeader = undefined
--     httpRequestHeader = undefined
--     httpEntityHeader = undefined
--     httpUnknownHeader = undefined

-- request :: Parser Request
-- request = do
--   (m, ru, v) <- requestLine <?> "request line"
--   hdrs <- many' (header <* crlf)
--   _ <- crlf
--   body <- option [] messageBody
--   return Request
--            { rqMethod  = m
--            , rqUri     = ru
--            , rqVersion = v
--            , rqHeaders = hdrs
--            , rqBody    = pack body
--            }

-- reasonPhraseText :: Parser Word8
-- reasonPhraseText = satisfy char_not_ctl
--   where char_not_ctl w = charPred w && not (ctlPred w)

-- reasonPhrase :: Parser [Word8]
-- reasonPhrase = many1 reasonPhraseText

-- statusLine :: Parser (HTTPVersion, Int, [Word8])
-- statusLine = (,,) <$> httpVersion <* sp
--                   <*> decimal <* sp
--                   <*> reasonPhrase <* crlf

-- response ::  Parser Response
-- response = do
--   (ver, code, reason) <- statusLine
--   hdrs <- many' (header <* crlf)
--   _ <- crlf
--   return Response
--     { rpCode = code
--     , rpHeaders = hdrs
--     , rpVersion = ver
--     , rpMessage = mempty
--     }

--   -- case lookup "Content-Length" hdrs of
--   --   Just n -> do msg <- option [] messageBody
--   --   Nothing -> error "Content-Length not found."

asList :: Parser a -> Parser [a]
asList p = (:[]) <$> p
