{-# LANGUAGE OverloadedStrings #-}

-- | Hypertext Transfer Protocol -- HTTP/1.1
-- <http://www.ietf.org/rfc/rfc2616.txt>
module Network.Parser.Rfc2616 where
--------------------------------------------------------------------------------
import           Control.Applicative      hiding (many)
import           Control.Monad            (liftM)
import           Data.Attoparsec
import           Data.Attoparsec.Char8    (decimal, stringCI, take)
import           Data.ByteString          as W hiding (concat, take)
import           Data.ByteString.Char8    as C hiding (concat, take)
import           Data.ByteString.Internal (c2w)
import           Data.Word                (Word8 ())
import           Prelude                  hiding (take, takeWhile)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2234
import           Network.Parser.Rfc3986   as R3986
import           Network.Parser.RfcCommon
import           Network.Types
--------------------------------------------------------------------------------
-- | * Basic Parser Constructs for RFC 2616
separatorsPred, tokenPred :: Word8 -> Bool

tokenPred w = charPred w && not (ctlPred w || separatorsPred w)

-- Page: 16
-- token = 1*<any CHAR except CTLs or separators>
token :: Parser [Word8]
token = many1 $ satisfy tokenPred
{-# INLINE token #-}

-- "()<>@,;:\\\"/[]?={} \t"
-- separatorSet :: [Word8]
-- separatorSet = [40,41,60,62,64,44,59,58,92,34,47,91,93,63,61,123,125,32,9]
separatorsPred = inClass "()<>@,;:\\\"/[]?={} \t" -- memberWord8 w (fromList separatorSet)


-- Page: 16
-- separators     = "(" | ")" | "<" | ">" | "@"
--                | "," | ";" | ":" | "\" | <">
--                | "/" | "[" | "]" | "?" | "="
--                | "{" | "}" | SP | HT
separators :: Parser Word8
separators = satisfy separatorsPred
{-# INLINE separators #-}

comment :: Parser [Word8]
comment
  = word8 40
    *> (concat <$> many' (quotedPair <|> ((:[]) <$> ctext)))
    <* word8 41

-- parse (httpVersion) (W.pack "HTTP/12.15\n")
httpVersion :: Parser HttpVersion
httpVersion = stringCI "http/" *>
              (HttpVersion <$> (num <* sep) <*> num)
 where num = liftM (read . C.unpack . W.pack) $ many1 digit
       sep = word8 . c2w $ '.'

-- parse (method) (W.pack "GET /")
method :: Parser Method
method = (GET         <$ stringCI "get")
         <|> (PUT     <$ stringCI "put")
         <|> (POST    <$ stringCI "post")
         <|> (HEAD    <$ stringCI "head")
         <|> (DELETE  <$ stringCI "delete")
         <|> (TRACE   <$ stringCI "trace")
         <|> (CONNECT <$ stringCI "connect")
         <|> (OPTIONS <$ stringCI "options")
         <|> ((EXTENSIONMETHOD . W.pack) <$> token)

requestUri :: Parser RequestUri
requestUri = try (Asterisk <$ word8 42)
             <|> RelativeRef <$> R3986.relativeRef
             <|> AbsoluteUri <$> R3986.absoluteUri
             <|> (AbsolutePath . W.pack) <$> R3986.pathAbsolute
             <|> Authority <$> R3986.authority

-- parse requestLine (C.pack "GET /my.cgi?foo=bar&john=doe HTTP/1.1\n")
requestLine :: Parser (Method, RequestUri, HttpVersion)
requestLine = ret <$> method      <* sp
                  <*> requestUri  <* sp
                  <*> httpVersion <* crlf
    where ret m u h = (m,u,h)

headerContentNcPred :: Word8 -> Bool
headerContentNcPred w
       = (w >= 0x00 && w <= 0x08)
      || (w >= 0x0b && w <= 0x0c)
      || (w >= 0x0e && w <= 0x1f)
      || (w >= 0x21 && w <= 0x39)
      || (w >= 0x3b && w <= 0xff)

headerContent :: Parser Word8
headerContent = satisfy (\w -> headerContentNcPred w || w == 58) -- ':'

headerName :: Parser [Word8]
headerName = many1 $ satisfy headerContentNcPred

headerValue :: Parser [Word8]
headerValue = do
  c <- headerContent
  r <- option [] (many' (headerContent <|> lws)) -- TODO: http://stuff.gsnedders.com/http-parsing.txt
  return (c:r)

header :: Parser (ByteString,ByteString)
header = ret <$> headerName  <* (word8 58 <* lwss)
             <*> headerValue <* lwss
    where ret n v = (W.pack n, W.pack v)

entityBody :: Parser [Word8]
entityBody = many' octet

messageBody :: Parser [Word8]
messageBody = entityBody

request :: Parser Request
request = do
  (m, ru, v) <- requestLine
  hdrs <- many' (header <* crlf)
  _ <- crlf
--  body <- option [] messageBody
  return Request
           { rqMethod  = m
           , rqUri     = ru
           , rqVersion = v
           , rqHeaders = hdrs
           , rqBody    = W.empty -- W.pack body
           }

reasonPhraseText :: Parser Word8
reasonPhraseText = satisfy char_not_ctl
  where char_not_ctl w = charPred w && not (ctlPred w)

reasonPhrase :: Parser [Word8]
reasonPhrase = many1 reasonPhraseText

statusLine :: Parser (HttpVersion, Int, [Word8])
statusLine = (,,) <$> httpVersion <* sp
                  <*> decimal <* sp
                  <*> reasonPhrase <* crlf

response ::  Parser Response
response = do
  (ver, code, reason) <- statusLine
  hdrs <- many' (header <* crlf)
  _ <- crlf
  return Response
    { rpCode = code
    , rpHeaders = hdrs
    , rpVersion = ver
    , rpMessage = W.empty
    }

  -- case lookup "Content-Length" hdrs of
  --   Just n -> do msg <- option [] messageBody
  --   Nothing -> error "Content-Length not found."

