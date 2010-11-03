{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Rfc2045 where

import Data.Attoparsec
import Data.ByteString as W
import qualified Data.ByteString.Char8 as C
import Test.Parser.Parser
import Test.HUnit
import Network.Http.Parser.Rfc2045
import Network.Http.Parser.Rfc2234
import Network.Http.Parser.RfcCommon

tests = TestList $ fmap TestCase lst
lst = [test_version, test_quotedPrintable, test_content]

test_version = ae "mime-version" (Just (1,0)) (aP version "Mime-Version: 1.0 ")

qpString = C.pack "If you believe that truth=3dbeauty, then surely=20=\nmathematics is the most beautiful branch of philosophy.\0"

qpStringRes = C.pack "If you believe that truth=beauty, then surely mathematics is the most beautiful branch of philosophy."

test_quotedPrintable = ae "quotedPrintable"
                       (Just qpStringRes)
                       (aP quotedPrintable qpString)

mimecontent1 = Content
test_content = ae "content" 
               (Just $ Content ())
               (aP content "Content-type: text/plain; charset=us-ascii (Plain text)")

-- *Network.Http.Parser.Rfc2045> parse content (C.pack "Content-Type: multipart/mixed; boundary=\"frontier\"\n")
-- Done "\n" Header {hType = ContentH, hValue = "multipart/mixed", hParams = fromList [("boundary","frontier")]}
