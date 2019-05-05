{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Test.Parser.Parser where
--------------------------------------------------------------------------------
import           Data.Attoparsec.ByteString
import           Data.ByteString            as W
import qualified Data.ByteString.Char8      as C
import           Data.Word                  (Word8)
import           Test.HUnit
--------------------------------------------------------------------------------

aP :: (Eq a) => Parser a -> ByteString -> Maybe a
aP p' i = case parse p' i of
            Done _ r -> return r
            _        -> fail "failed"

-- Utils
ae :: (Eq a, Show a) => String -> a -> a -> Assertion
ae = assertEqual
