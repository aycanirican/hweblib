{-# LANGUAGE OverloadedStrings #-}
module Main where
--------------------------------------------------------------------------------
import Criterion.Main (defaultMain, bgroup, bench, whnf)
import Data.Attoparsec (parseOnly, parse)
--------------------------------------------------------------------------------
import Test.Parser.Rfc7230
import Network.Parser.Rfc3986 (uri)
import Network.Http
--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain [rfc7230, rfc3986]

-- taken from a random internet site
testUri = "http://username:password@www.hti.umich.edu:8080/cgi/t/text/pageviewer-idx?c=umhistmath;cc=umhistmath;rgn=full+text;idno=ABS3153.0001.001;didno=ABS3153.0001.001;view=image;seq=00000140;some-too-long-query-string-so-that-the-whole-url-is-longer-than-255-characters;some-too-long-query-string-so-that-the-whole-url-is-longer-than-255-characters;some-too-long-query-string-so-that-the-whole-url-is-longer-than-255-characters;some-too-long-query-string-so-that-the-whole-url-is-longer-than-255-characters\n"

rfc3986 = bgroup "rfc3986"
          [ bench "uri" $ whnf (parse uri) testUri]

rfc7230 = bgroup "rfc7230"
          [ bench "request" $ whnf (parseOnly parseMessage) getData2]

