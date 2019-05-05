{-# LANGUAGE OverloadedStrings #-}
module Main where

--------------------------------------------------------------------------------
import           System.Environment     (getArgs)
import           Test.HUnit
--------------------------------------------------------------------------------

-- implementations
import qualified Network.Parser.Rfc2045 as R2045
import qualified Network.Parser.Rfc2822 as R2822
import qualified Network.Parser.Rfc3986 as R3986
import qualified Network.Parser.Rfc7230 as R7230

-- tests
import           Test.Parser.Rfc2045    as T2045
import           Test.Parser.Rfc2822    as T2822
import           Test.Parser.Rfc3986    as T3986
import           Test.Parser.Rfc7230    as T7230

main = do
  runTestTT T3986.tests
  runTestTT T7230.tests
  runTestTT T2822.tests
  runTestTT T2045.tests
