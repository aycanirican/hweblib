{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Main where

import System.Environment (getArgs)
import Test.HUnit

-- implementations
import qualified Network.Parser.Rfc2045 as R2045
import qualified Network.Parser.Rfc2822 as R2822
import qualified Network.Parser.Rfc2616 as R2616
import qualified Network.Parser.Rfc3986 as R3986

-- tests
import Test.Parser.Rfc3986 as T3986
import Test.Parser.Rfc2616 as T2616
import Test.Parser.Rfc2045 as T2045
import Test.Parser.Rfc2822 as T2822

main = do
  runTestTT T3986.tests
  runTestTT T2616.tests
  runTestTT T2822.tests
  runTestTT T2045.tests
