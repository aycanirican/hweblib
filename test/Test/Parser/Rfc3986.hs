{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Rfc3986 where

import Data.Attoparsec
import qualified Data.ByteString.Char8 as C
import Network.Parser.RfcCommon
import Test.Parser.Parser
import Test.HUnit
import Network.Types
import Network.Parser.Rfc3986

tests = TestList $ fmap TestCase lst

lst = [test_simple_query, test_simple_query, test_uri]
test_uri 
    = let i = "http://user:pass@www.core.gen.tr/dir1/file.ext?param1=val1&param2=val2 "
          o = Just URI { uriScheme = "http"
                       , uriAuthority = Just $ URIAuth "user:pass" "www.core.gen.tr" ""
                       , uriPath = "/dir1/file.ext"
                       , uriQuery = "?param1=val1&param2=val2"
                       , uriFragment = ""
                       }
      in
        assertEqual "uri" o (aP uri i)

test_simple_query =
  let i = "foo=bar&zoo=yoo "
      o = Just "?foo=bar&zoo=yoo"
  in assertEqual "simple_query" o (fmap toRepr (aP query i))

test_simple_query2 =
  let i = "foo=bar;zoo=yoo "
      o = Just "?foo=bar;zoo=yoo"
  in assertEqual "simple_query2" o (fmap toRepr (aP query i))

