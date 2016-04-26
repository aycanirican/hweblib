{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.7230
-- Copyright   :  Aycan iRiCAN 2010-2015
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
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad                    (join, liftM, liftM2)
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString                  hiding (count)
import qualified Data.ByteString.Char8            as BSC
import           Data.ByteString.Internal         (c2w)
import           Data.Monoid
import           Data.Scientific
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Word                        (Word8)
import           Numeric                          (readHex)
--------------------------------------------------------------------------------
import qualified Network.Parser.Rfc3986           as R3986
import           Network.Parser.Rfc5234
import           Network.Types
--------------------------------------------------------------------------------

data StartLine
  = RequestLine ByteString RequestTarget HTTPVersion
  | StatusLine HTTPVersion ByteString ByteString
    deriving (Eq, Show)

data HTTPMessage
  = HTTPMessage { status  :: StartLine
                , headers :: [(ByteString, ByteString)]
                , body    :: Maybe ByteString
                } deriving (Eq, Show)

data TransferCoding
  = Chunked
  | Compress
  | Deflate
  | GZip
  | TransferExtension ByteString [(ByteString, ByteString)]
    deriving (Eq, Show)

data Chunk
  = Chunk Int [(ByteString, ByteString)] ByteString
    deriving (Eq, Show)

data TrailerPart
  = TrailerPart [(ByteString, ByteString)]
    deriving (Eq, Show)

data ChunkedBody
  = ChunkedBody [Chunk] TrailerPart
    deriving (Eq, Show)

data TCoding
  = TCoding TransferCoding (Maybe Scientific)
  | Trailers
    deriving (Eq, Show)

data RequestTarget
  = OriginForm ByteString ByteString
  | AbsoluteForm URI
  | AuthorityForm (Maybe URIAuth)
  | AsteriskForm
    deriving (Eq, Show)

data Protocol
  = ProtocolName ByteString
  | ProtocolVersion ByteString
  | ProtocolNameVersion ByteString ByteString deriving (Eq, Show)

data Via = Via [(Protocol, ReceivedBy)] deriving (Eq, Show)

data ReceivedBy
  = ReceivedByHost ByteString (Maybe ByteString) -- ^ hostname port
  | ReceivedByPseudonym ByteString       -- ^ token
    deriving (Eq, Show)

-- >>> parse http_version "HTTP/1.1"
-- Done "" (HTTPVersion {httpMajor = 1, httpMinor = 1})
http_version :: Parser HTTPVersion
http_version
  = AC.stringCI "http/1.0" *> return (HTTPVersion 1 0)
  <|> AC.stringCI "http/1.1" *> return (HTTPVersion 1 1)

http_message :: Parser HTTPMessage
http_message = do
  sl <- start_line
  xs <- many (header_field <* crlf)
  crlf
  m <- option Nothing (Just <$> message_body)
  return $ HTTPMessage sl xs m

-- 2.7.  Uniform Resource Identifiers
absolute_path :: Parser [ByteString]
absolute_path = fmap pack <$> many1 (AC.char '/' *> R3986.segment)

-- 3.1.  Start Line
start_line :: Parser StartLine
start_line = request_line <|> status_line

-- 3.1.1.  Request Line
request_line :: Parser StartLine
request_line = RequestLine <$> method <* sp <*> request_target <* sp <*> http_version <* crlf

method :: Parser ByteString
method = token

-- 3.1.2.  Status Line
status_line :: Parser StartLine
status_line = StatusLine <$> (http_version <* sp) <*> status_code <* sp <*> reason_phrase <* crlf

status_code :: Parser ByteString
status_code = pack <$> manyN 3 digit

reason_phrase :: Parser ByteString
reason_phrase = pack <$> many (htab <|> sp <|> vchar <|> obs_text)

-- 3.2.  Header Fields
header_field :: Parser (ByteString, ByteString)
header_field = (,) <$> (field_name <* word8 58 <* ows) <*> field_value <* ows

field_name :: Parser ByteString
field_name = token

field_value :: Parser ByteString
field_value = BSC.concat <$> many (field_content <|> obs_fold)


-- | Slightly modified version to catch below example.
-- >>> parseOnly field_content "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:29.0) Gecko/20100101 Firefox/29.0"
-- Right "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:29.0) Gecko/20100101 Firefox/29.0"
field_content :: Parser ByteString
field_content = do
  x <- field_vchar
  xs <- option mempty (many (sp <|> htab <|> field_vchar))

  return $ pack (x:xs)

field_vchar :: Parser Word8
field_vchar = vchar <|> obs_text

obs_fold :: Parser ByteString
obs_fold = ret <$> (asList (ows *> crlf)) <*> (many1 (sp <|> htab))
  where ret x y = pack (x ++ y)

-- 3.2.3.  Whitespace
ows, rws, bws :: Parser [Word8]
ows = many (sp <|> htab)
rws = many1 (sp <|> htab)
bws = ows

-- 3.2.5.  Field Limits
token :: Parser ByteString
token = pack <$> many1 tchar

tchar :: Parser Word8
tchar = satisfy pred <|> digit <|> alpha
  where pred = inClass "!#$%&'*+-.^_`|~"

quoted_string :: Parser ByteString
quoted_string = ret . join <$> (dquote *> many (asList qdtext <|> quoted_pair) <* dquote)
  where
    ret x = "\"" <> (pack x) <>  "\""

qdtext :: Parser Word8
qdtext = htab <|> sp <|> satisfy pred <|> obs_text
  where
    pred w = w == 0x21
             || (w >= 0x23 && w <= 0x5b)
             || (w >= 0x5d && w <= 0x7e)

obs_text :: Parser Word8
obs_text = satisfy (\w -> w >= 0x80 && w <= 0xff)

comment :: Parser ByteString
comment = ret <$> (   word8 40
                      *> many (fmap singleton ctext
                               <|> fmap pack quoted_pair
                               <|> comment)
                      <* word8 41)
  where ret x = "(" <> BSC.concat x <> ")"

ctext :: Parser Word8
ctext = htab <|> sp <|> satisfy pred <|> obs_text
  where pred w = (w >= 0x21 && w <= 0x27)
                 || (w >= 0x2a && w <= 0x5b)
                 || (w >= 0x5d && w <= 0x7e)

quoted_pair :: Parser [Word8]
quoted_pair = (\a b -> a:b:[]) <$> word8 92 <*> (htab <|> sp <|> vchar <|> obs_text)

-- 3.3.  Message Body
message_body :: Parser ByteString
message_body = pack <$> many octet

-- 3.3.1.  Transfer-Encoding
transfer_encoding = dash1 transfer_coding

-- 3.3.2.  Content-Length
content_length :: Parser Int
content_length = either (fail "Not an integer") id . floatingOrInteger <$> AC.scientific
-- 4.  Transfer Codings
transfer_coding :: Parser TransferCoding
transfer_coding
  =     AC.stringCI "chunked"  *> return Chunked
    <|> AC.stringCI "compress" *> return Compress
    <|> AC.stringCI "deflate"  *> return Deflate
    <|> AC.stringCI "gzip"     *> return GZip
    <|> transfer_extension

transfer_extension :: Parser TransferCoding
transfer_extension
  = TransferExtension <$> token
                      <*> many (ows *> AC.char ';' *> ows *> transfer_parameter)

transfer_parameter :: Parser (ByteString, ByteString)
transfer_parameter = (,) <$> (token <* bws <* AC.char '=') <*> (bws *> (token <|> quoted_string))

-- 4.1.  Chunked Transfer Coding

chunked_body :: Parser ChunkedBody
chunked_body = ChunkedBody <$> (manyTill chunk last_chunk) <*> (trailer_part <* crlf)

chunk = do
  n <- chunk_size
  ext <- option [] chunk_ext <* crlf
  d <- count n octet <* crlf
  return $ Chunk n ext (pack d)

chunk_size :: Parser Int
chunk_size = do
  a <- readHex . BSC.unpack . pack <$> many1 hexdig
  case a of
    [] -> fail "invalid chunk size"
    [(i,_)] -> return i
    _ -> fail "invalid chunk size"

last_chunk :: Parser Chunk
last_chunk = lastChunk <$> (many1 (AC.char '0') *> option [] chunk_ext <* crlf)
  where lastChunk ext = Chunk 0 ext mempty

chunk_data :: Parser ByteString
chunk_data = pack <$> many1 octet


-- 4.1.1.  Chunk Extensions
chunk_ext :: Parser [(ByteString, ByteString)]
chunk_ext = many ((,) <$> (AC.char ';' *> ows *> chunk_ext_name)
                      <*> option mempty (AC.char '=' *> chunk_ext_val))

chunk_ext_name :: Parser ByteString
chunk_ext_name = token

chunk_ext_val :: Parser ByteString
chunk_ext_val = token <|> quoted_string

-- 4.1.2.  Chunked Trailer Part
trailer_part :: Parser TrailerPart
trailer_part = TrailerPart <$> many (header_field <* crlf)

-- 4.3.  TE
te :: Parser [TCoding]
te = dash t_codings

t_codings :: Parser TCoding
t_codings
  = (const Trailers <$> AC.stringCI "trailers")
    <|> (TCoding <$> transfer_coding <*> option Nothing (Just <$> t_ranking))

t_ranking :: Parser Scientific
t_ranking = ows *> AC.char ';' *> ows *> AC.stringCI "q=" *> rank

rank :: Parser Scientific
rank = (AC.char '0' *> option 0 prec)
   <|> (AC.char '1' *> option 1 (AC.char '.' *> manyNtoM 0 3 (AC.char '0') *> return 1))
  where
    prec :: Parser Scientific
    prec = do
      x <- parseOnly AC.scientific . pack . ([48,46] ++) <$> (AC.char '.'  *> manyNtoM 0 3 digit)
      case x of
        Left  str -> fail str
        Right sci -> return sci

-- 4.4.  Trailer
trailer = dash1 field_name

-- 5.3.  Request Target
request_target
  =     absolute_form
    <|> (const AsteriskForm  <$> asterisk_form)
    <|> origin_form
    <|> authority_form

-- 5.3.1.  origin-form
origin_form :: Parser RequestTarget
origin_form = OriginForm <$> R3986.pathAbsolute <*> option mempty (AC.char '?' *> R3986.query)

-- 5.3.2.  absolute-form
absolute_form :: Parser RequestTarget
absolute_form = AbsoluteForm <$> R3986.absoluteUri

-- 5.3.3.  authority-form
authority_form :: Parser RequestTarget
authority_form = AuthorityForm <$> R3986.authority

-- 5.3.4.  asterisk-form
asterisk_form :: Parser RequestTarget
asterisk_form = AC.char '*' >> return AsteriskForm

-- 5.4.  Host
host :: Parser (ByteString, Maybe ByteString)
host = (,) <$> uri_host <*> option Nothing (Just <$> (AC.char ':' *> port))

port = R3986.port
uri_host = R3986.host

-- 5.7.  Message Forwarding

-- 5.7.1.  Via
via :: Parser Via
via = Via <$> dash1 ((,) <$> (received_protocol <* rws)
                         <*> received_by <* option mempty (rws <* comment))

received_protocol :: Parser Protocol
received_protocol
  = do n <- option Nothing (fmap Just (protocol_name <* AC.char '/'))
       v <- protocol_version
       return $ case n of
          Just n' -> ProtocolNameVersion n' v
          Nothing -> ProtocolVersion v

received_by :: Parser ReceivedBy
received_by = (uncurry ReceivedByHost <$> host)
              <|> (ReceivedByPseudonym <$> pseudonym)

pseudonym :: Parser ByteString
pseudonym = token

-- 6.  Connection Management
-- 6.1.  Connection
connection :: Parser [ByteString]
connection = dash1 connection_option

connection_option :: Parser ByteString
connection_option = token

-- 6.7.  Upgrade
upgrade :: Parser [Protocol]
upgrade = dash1 protocol

protocol :: Parser Protocol
protocol = do n <- protocol_name
              v <- option Nothing (Just <$> (AC.char '/' *> protocol_version))
              return $ case v of
                Just v' -> ProtocolNameVersion n v'
                Nothing -> ProtocolName n

protocol_name :: Parser ByteString
protocol_name = token

protocol_version :: Parser ByteString
protocol_version = token

-- 7.  ABNF List Extension: #rule
{- A #rule extension to the ABNF rules of [RFC5234] is used to improve
   readability in the definitions of some header field values.

   A construct "#" is defined, similar to "*", for defining
   comma-delimited lists of elements.  The full form is "<n>#<m>element"
   indicating at least <n> and at most <m> elements, each separated by a
   single comma (",") and optional whitespace (OWS). -}

dash :: Parser a -> Parser [a]
dash p = (:) <$> p <*> many (ows *> word8 44 *> ows *> p)

dash1 :: Parser a -> Parser [a]
dash1 = dashN 1

dashN :: Int -> Parser a -> Parser [a]
dashN 0 p = return []
dashN n p
  | n == 0    = return []
  | otherwise = (\a b c -> a:(b++c)) <$> p
                                     <*> count (n-1) (addSep p)
                                     <*> many        (addSep p)
  where addSep p = ows *> word8 44 *> ows *> p

dashNtoM :: Int -> Int -> Parser a -> Parser [a]
dashNtoM n m p
  | n < 0     = return []
  | n > m     = return []
  | n == m    = (:) <$> p <*> count (n-1) (sep *> p)
  | n == 0    = sepByMaxN m p sep
  | otherwise = (++) <$> sepByN n p sep <*> (sep *> dashNtoM 0 (m-n) p)
  where
    sep = (ows *> word8 44 *> ows *> return ())

-- | Parse `n` times separated by `s`
sepByN :: Int -> Parser a -> Parser () -> Parser [a]
sepByN 0 p s = return mempty
sepByN n p s = (:) <$> p <*> count (n-1) (s *> p)

sepByMaxN :: Int -> Parser a -> Parser () -> Parser [a]
sepByMaxN 0 p s = return mempty
sepByMaxN n p s = ((:) <$> p <*> maxP (n-1) (s *> p)) <|> return mempty

-- | Parse `p` at max `n` times
maxP :: Int -> Parser a -> Parser [a]
maxP 0 p = return mempty
maxP n p = ((:) <$> p <*> maxP (n-1) p) <|> return mempty

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

