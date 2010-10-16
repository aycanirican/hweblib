{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Rfc2616 where

import Data.Attoparsec
import Data.ByteString as W
import qualified Data.ByteString.Char8 as C
import Test.Parser.Parser
import Test.HUnit
import Network.Http.Parser.Rfc2616

-- Utils
ae t a b = assertEqual t a b
p = Just . W.unpack . C.pack

tests = TestList $ fmap TestCase lst

lst = [test_octet, test_char, test_upalpha, test_loalpha, test_lws, test_quotedString]
-- Tests

test_octet = ae "octet" (Just 48) (aP octet "01")
test_char = ae "char" (Just 65) (aP char "AbC")
test_upalpha = ae "upalpha" (Just 66) (aP upalpha "Ba")
test_loalpha = ae "loalpha" (Just 122) (aP loalpha "zE")

test_lws = ae "lws" (Just 32) (aP lws "\n\t   asd")
test_quotedString = ae "quotedString" (p "Take this") (aP quotedString "\"Take this\"")
