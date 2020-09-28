{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Rfc3986 where

import Data.Attoparsec.ByteString (parseOnly)
import Network.Parser.Rfc3986 (query, uri)
import Network.Types
  ( URI
      ( URI,
        uriAuthority,
        uriFragment,
        uriPath,
        uriQuery,
        uriScheme
      ),
    URIAuth (URIAuth, uriPort, uriRegName, uriUserInfo),
  )
import Test.HUnit
  ( Assertion,
    Test (TestCase, TestList),
    assertEqual,
  )
import Test.Parser.Parser ()

tests = TestList $ fmap TestCase lst

lst :: [Assertion]
lst = [ test_simple_query
      , test_simple_query2
      , test_uri ]

test_uri = assertEqual "uri" (Right o) (parseOnly uri i)
  where
    i = "http://user:pass@www.core.gen.tr/dir1/file.ext?param1=val1&param2=val2 "
    o = (URI { uriScheme = "http"
             , uriAuthority = Just (URIAuth { uriUserInfo = "user:pass"
                                            , uriRegName = "www.core.gen.tr"
                                            , uriPort = ""})
             , uriPath = "/dir1/file.ext"
             , uriQuery = "param1=val1&param2=val2"
             , uriFragment = ""})

test_simple_query
  = assertEqual "simple_query"
    (Right "foo=bar&zoo=yoo")
    (parseOnly query "foo=bar&zoo=yoo ")

test_simple_query2
  = assertEqual "simple_query2"
     (Right "foo=bar;zoo=yoo")
     (parseOnly query "foo=bar;zoo=yoo ")

