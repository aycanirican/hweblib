{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Rfc2616 where

import Data.Attoparsec
import Data.ByteString as W
import qualified Data.ByteString.Char8 as C
import Test.Parser.Parser
import Test.HUnit
import Network.Http.Parser.Rfc2616
import Network.Http.Parser.RfcCommon

-- Data
parseGetData = C.concat 
               [ "GET /favicon.ico HTTP/1.1\r\n"
               , "Host: 0.0.0.0=5000\r\n"
               , "User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0\r\n"
               , "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
               , "Accept-Language: en-us,en;q=0.5\r\n"
               , "Accept-Encoding: gzip,deflate\r\n"
               , "Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n"
               , "Keep-Alive: 300\r\n"
               , "Connection: keep-alive\r\n"
               , "\r\n" ]

tests = TestList $ fmap TestCase lst

lst = [test_octet, test_char, test_upalpha, test_loalpha, test_lws, test_quotedString, test_requestline, test_requestline2]
-- Tests

test_octet = ae "octet" (Just 48) (aP octet "01")
test_char = ae "char" (Just 65) (aP char "AbC")
test_upalpha = ae "upalpha" (Just 66) (aP upalpha "Ba")
test_loalpha = ae "loalpha" (Just 122) (aP loalpha "zE")

test_lws = ae "lws" (Just 32) (aP lws "\n\t   asd")
test_quotedString = ae "quotedString" (p "Take this") (aP quotedString "\"Take this\"")


test_requestline = ae "requestLine" 
                   (Just (GET,Asterisk,(1,1)))
                   (aP requestLine "GET * HTTP/1.1\n")

test_requestline2 = ae "requestLine" 
                    (Just (GET,AbsolutePath "/index.html",(1,1)))
                    (aP requestLine "GET /index.html HTTP/1.1\n")

test_request = ae "request"
               (Just $ Request {rqMethod = GET, rqUri = AbsolutePath "/favicon.ico", rqVersion = (1,1), rqHeaders = [("Host","0.0.0.0=5000"),("User-Agent","Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0"),("Accept","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),("Accept-Language","en-us,en;q=0.5"),("Accept-Encoding","gzip,deflate"),("Accept-Charset","ISO-8859-1,utf-8;q=0.7,*;q=0.7"),("Keep-Alive","300"),("Connection","keep-alive")], rqBody = ""})
               (aP request parseGetData)