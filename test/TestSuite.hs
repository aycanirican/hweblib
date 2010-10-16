{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Main where

import System.Environment (getArgs)
import Test.HUnit

-- implementations
import qualified Network.Http.Parser.Rfc2616 as R2616
import qualified Network.Http.Parser.Rfc3986 as R3986

-- tests
import Test.Parser.Rfc3986 as T3986
import Test.Parser.Rfc2616 as T2616

rfc3986Tests = TestList [ TestCase T3986.test_uri
                        ]
rfc2616Tests = TestList [ TestCase T2616.test_octet
                        , TestCase T2616.test_char
                        ]

main = do
  runTestTT rfc3986Tests
  runTestTT rfc2616Tests
