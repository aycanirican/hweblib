{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Rfc2616 where
--------------------------------------------------------------------------------
import           Data.Attoparsec
import           Data.ByteString          as W
import qualified Data.ByteString.Char8    as C
import           Test.HUnit
import           Test.Parser.Parser
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2234
import           Network.Parser.Rfc2616
import           Network.Parser.RfcCommon
import           Network.Types
--------------------------------------------------------------------------------
-- Data
parseGetData :: ByteString
parseGetData = "GET /favicon.ico HTTP/1.1\r\n\
                \Host: 0.0.0.0=5000\r\n\
                \User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0\r\n\
                \Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
                \Accept-Language: en-us,en;q=0.5\r\n\
                \Accept-Encoding: gzip,deflate\r\n\
                \Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n\
                \Keep-Alive: 300\r\n\
                \Connection: keep-alive\r\n\
                \\r\n"

tests :: Test
tests = TestList $ fmap TestCase lst

lst = [testOctet, testChar, testUpalpha, testLoalpha, testLws, testQuotedString, testRequestline, testRequestline2]
-- Tests

testOctet = ae "octet" (Just 48) (aP octet "01")
testChar = ae "char" (Just 65) (aP char "AbC")
testUpalpha = ae "upalpha" (Just 66) (aP upalpha "Ba")
testLoalpha = ae "loalpha" (Just 122) (aP loalpha "zE")

testLws = ae "lws" (Just 32) (aP lws "\n\t   asd")
testQuotedString = ae "quotedString" (p "Take this") (aP quotedString "\"Take this\"")


testRequestline = ae "requestLine"
                  (Just (GET, Asterisk, http11))
                  (aP requestLine "GET * HTTP/1.1\n")

testRequestline2 = ae "requestLine"
                   (Just (GET, AbsolutePath "/index.html", http11))
                   (aP requestLine "GET /index.html HTTP/1.1\n")

testRequestline3 = ae "requestLine"
                   (Just (GET, RelativeRef URI { uriScheme = ""
                                               , uriAuthority = Nothing
                                               , uriPath = "/my.cgi"
                                               , uriQuery = "?foo=bar&john=doe"
                                               , uriFragment = ""}
                                               , http11))
                   (aP requestLine "GET /my.cgi?foo=bar&john=doe HTTP/1.1\n")

testRequest = ae "request"
              (Just Request { rqMethod = GET
                            , rqUri = AbsolutePath "/favicon.ico"
                            , rqVersion = http11
                            , rqHeaders = [ ("Host","0.0.0.0=5000")
                                          , ("User-Agent","Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0")
                                          , ("Accept","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                                          , ("Accept-Language","en-us,en;q=0.5")
                                          , ("Accept-Encoding","gzip,deflate")
                                          , ("Accept-Charset","ISO-8859-1,utf-8;q=0.7,*;q=0.7")
                                          , ("Keep-Alive","300")
                                          , ("Connection","keep-alive")]
                                          , rqBody = ""})
              (aP request parseGetData)

responseString :: ByteString
responseString = "HTTP/1.1 200 OK\r\n\
                 \Content-Type: text/plain\r\n\
                 \Content-Length: 13\r\n\
                 \\r\n\
                 \Heartbeat OK!\r\n"

testResponse = ae "response"
             (Just Response { rpCode = 200
                            , rpHeaders = [ ("Content-Type", "text/plain")
                                          , ("Content-Length", "13") ]
                            , rpVersion = http11
                            , rpMessage = ""
                            })
             (aP response responseString)
