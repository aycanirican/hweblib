{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Parser where
--------------------------------------------------------------------------------
import Data.Attoparsec
import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString.Char8 as C
import Data.ByteString as W
import Data.Word
import Test.HUnit
--------------------------------------------------------------------------------
aP :: (Eq a) => Parser a -> ByteString -> Maybe a
aP p i = case parse p i of
           Done _ r -> return r
           _        -> fail "failed"

-- Utils
ae t a b = assertEqual t a b
p = Just . W.unpack . C.pack
