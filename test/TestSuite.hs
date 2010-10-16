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

main = do
  runTestTT T3986.tests
  runTestTT T2616.tests
