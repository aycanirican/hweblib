-- -*- dante-target: "hunit"; -*-
{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Rfc7230 where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Network.Parser.Rfc7230
  ( HTTPMessage (HTTPMessage, body, headers, status),
    RequestTarget (AsteriskForm, OriginForm),
    StartLine (RequestLine, StatusLine),
    httpMessage,
    requestLine,
  )
import Network.Types
  ( HTTPVersion (HTTPVersion, httpMajor, httpMinor),
  )
import Test.HUnit (Assertion, Test (TestCase, TestList))
import Test.Parser.Parser (ae)

tests :: Test
tests = TestList $ fmap TestCase lst

lst :: [Assertion]
lst = [ testAsteriskFormRequestLine
      , testAbsolutePathRequestLine
      , testRequestWithQuery
      , testGetRequestWithHeaders
      , testGetRequestWithQuery
      , testSimpleResponse
      ]

testAsteriskFormRequestLine :: Assertion
testAsteriskFormRequestLine
  = ae "AsteriskFormRequestLine"
    (Right (RequestLine "GET" AsteriskForm (HTTPVersion {httpMajor = 1, httpMinor = 1})))
    (parseOnly requestLine "GET * HTTP/1.1\n")

testAbsolutePathRequestLine :: Assertion
testAbsolutePathRequestLine
  = ae "AbsolutePathRequestLine"
  (Right (RequestLine "GET"
          (OriginForm "/index.html" "")
          (HTTPVersion {httpMajor = 1, httpMinor = 1})))
  (parseOnly requestLine "GET /index.html HTTP/1.1\n")

testRequestWithQuery :: Assertion
testRequestWithQuery
  = ae "RequestWithQuert"
    (Right (RequestLine "GET"
            (OriginForm "/my.cgi" "foo=bar&john=doe")
            (HTTPVersion {httpMajor = 1, httpMinor = 1})))
    (parseOnly requestLine "GET /my.cgi?foo=bar&john=doe HTTP/1.1\n")

testGetRequestWithHeaders :: Assertion
testGetRequestWithHeaders
  = ae "GetRequestWithHeaders"
    (Right (HTTPMessage { status = RequestLine "GET"
                                   (OriginForm "/favicon.ico" "")
                                   (HTTPVersion {httpMajor = 1, httpMinor = 1})
                        , headers = [ ("Host","0.0.0.0")
                                    , ("User-Agent","Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0")
                                    , ("Accept","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                                    , ("Accept-Language","en-us,en;q=0.5")
                                    , ("Accept-Encoding","gzip,deflate")
                                    , ("Accept-Charset","ISO-8859-1,utf-8;q=0.7,*;q=0.7")
                                    , ("Keep-Alive","300")
                                    , ("Connection","keep-alive")
                                    ]
                        , body = Nothing }))
    (parseOnly httpMessage getData1)

testGetRequestWithQuery :: Assertion
testGetRequestWithQuery
  = ae "GetRequestWithQuery"
    (Right (HTTPMessage { status = RequestLine "GET"
                                   (OriginForm "/search" "q=haskell")
                                   (HTTPVersion {httpMajor = 1, httpMinor = 1})
                        , headers = [ ("Host","www.google.co.jp")
                                    , ("User-Agent","Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:29.0) Gecko/20100101 Firefox/29.0")
                                    , ("Accept","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                                    , ("Accept-Language","ja,en-us;q=0.7,en;q=0.3")
                                    , ("Accept-Encoding","gzip, deflate")
                                    , ("Cookie","PREF=ID=7ca99b92f2f9afe9:FF=0:TM=1400941989:LM=1400941989:S=w2rcVppuXeOj6pXi; NID=67=N3XLJvzIIHqfvn_LuNTgBmA59ni-8YNTZ-RFASwaaByYQ9q0wfljtVHijfzQIGVJQ8axRzvOoBI9idcimOPUVI4obXncxIUUVf17AgubzR8KFBQVlXb7n2S0LNV43EPV")
                                    , ("Connection","keep-alive") ]
                        , body = Nothing }))

    (parseOnly httpMessage getData2)

responseString :: ByteString
responseString = "HTTP/1.1 200 OK\r\n\
                 \Content-Type: text/plain\r\n\
                 \Content-Length: 13\r\n\
                 \\r\n\
                 \Heartbeat OK!\r\n"

testSimpleResponse :: Assertion
testSimpleResponse
  = ae "SimpleResponse"
    (Right (HTTPMessage { status = StatusLine (HTTPVersion {httpMajor = 1, httpMinor = 1}) "200" "OK"
                        , headers = [ ("Content-Type","text/plain")
                                    , ("Content-Length","13") ]
                        , body = Just "Heartbeat OK!\r\n"}))
    (parseOnly httpMessage responseString)

-- Data
getData1 :: ByteString
getData1
  = "GET /favicon.ico HTTP/1.1\r\n\
    \Host: 0.0.0.0\r\n\
    \User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0\r\n\
    \Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
    \Accept-Language: en-us,en;q=0.5\r\n\
    \Accept-Encoding: gzip,deflate\r\n\
    \Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n\
    \Keep-Alive: 300\r\n\
    \Connection: keep-alive\r\n\
    \\r\n"

getData2 :: ByteString
getData2
  = "GET /search?q=haskell HTTP/1.1\r\n\
    \Host: www.google.co.jp\r\n\
    \User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:29.0) Gecko/20100101 Firefox/29.0\r\n\
    \Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
    \Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n\
    \Accept-Encoding: gzip, deflate\r\n\
    \Cookie: PREF=ID=7ca99b92f2f9afe9:FF=0:TM=1400941989:LM=1400941989:S=w2rcVppuXeOj6pXi; NID=67=N3XLJvzIIHqfvn_LuNTgBmA59ni-8YNTZ-RFASwaaByYQ9q0wfljtVHijfzQIGVJQ8axRzvOoBI9idcimOPUVI4obXncxIUUVf17AgubzR8KFBQVlXb7n2S0LNV43EPV\r\n\
    \Connection: keep-alive\r\n\
    \\r\n"

