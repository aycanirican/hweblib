{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Test.Parser.Rfc3986 where

import Data.Attoparsec
import qualified Data.ByteString.Char8 as C
import Test.Parser.Parser
import Test.HUnit
import Network.Parser.Rfc3986

tests = TestList $ fmap TestCase lst

lst = [test_uri]
test_uri 
    = let i = "http://user:pass@www.core.gen.tr/dir1/file.ext?param1=val1&param2=val2 "
          o = Just $ URI { uriScheme = "http"
                         , uriAuthority = Just $ URIAuth "user:pass" "www.core.gen.tr" ""
                         , uriPath = "/dir1/file.ext"
                         , uriQuery = "?param1=val1&param2=val2"
                         , uriFragment = ""
                         }
      in
        assertEqual "uri" o (aP uri i)
