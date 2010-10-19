{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Rfc2045 where

import Data.Attoparsec
import Data.ByteString as W
import qualified Data.ByteString.Char8 as C
import Test.Parser.Parser
import Test.HUnit
import Network.Http.Parser.Rfc2045
import Network.Http.Parser.RfcCommon

tests = TestList $ fmap TestCase lst
lst = [test_mimeVersion]

test_mimeVersion = ae "mimeVersion" (Just (1,0)) (aP mimeVersion "Mime-Version: 1.0 ")
