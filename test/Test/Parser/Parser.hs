{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Test.Parser.Parser where

import Data.Attoparsec
import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString.Char8 as C
import Data.ByteString
import Data.Word

aP :: (Eq a) => Parser a -> ByteString -> Maybe a
aP p i = case (parse p i) of
           Done _ r -> return r
           _        -> fail "failed"

