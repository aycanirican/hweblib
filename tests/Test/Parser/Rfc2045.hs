{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Rfc2045 where

import qualified Data.Map as M
import Network.Parser.Rfc2045
  ( Header (Header),
    HeaderType (VersionH),
    quoted_printable,
    version,
  )
import Test.HUnit (Test (TestCase, TestList))
import Test.Parser.Parser (aP, ae)

tests = TestList $ fmap TestCase lst

lst = [ test_version
      , test_quotedPrintable]

test_version = ae "mime-version" (Just (Header VersionH "1.0" M.empty)) (aP version "Mime-Version: 1.0 ")

qpString = "If you believe that truth=3dbeauty, then surely=20=\nmathematics is the most beautiful branch of philosophy.\0"
qpStringRes = "If you believe that truth=beauty, then surely \nmathematics is the most beautiful branch of philosophy."

test_quotedPrintable = ae "quotedPrintable"
                       (Just qpStringRes)
                       (aP quoted_printable qpString)

--mimecontent1 = Content
--test_content = ae "content"
--               (Just $ Content ())
--               (aP content "Content-type: text/plain; charset=us-ascii (Plain text)")

-- *Network.Http.Parser.Rfc2045> parse content (C.pack "Content-Type: multipart/mixed; boundary=\"frontier\"\n")
-- Done "\n" Header {hType = ContentH, hValue = "multipart/mixed", hParams = fromList [("boundary","frontier")]}
