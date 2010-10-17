module Main where

import Criterion.Main
import Network.Http.Parser.Rfc3986 (uri)
import Network.Http.Parser.Rfc2616 (request)
import Data.Attoparsec
import Data.ByteString.Char8 as C
import Test.Parser.Rfc2616 (parseGetData)

main :: IO ()
main = defaultMain [rfc2616, rfc3986]

-- taken from a random internet site
testUri = (C.pack "http://username:password@www.hti.umich.edu:8080/cgi/t/text/pageviewer-idx?c=umhistmath;cc=umhistmath;rgn=full+text;idno=ABS3153.0001.001;didno=ABS3153.0001.001;view=image;seq=00000140;some-too-long-query-string-so-that-the-whole-url-is-longer-than-255-characters;some-too-long-query-string-so-that-the-whole-url-is-longer-than-255-characters;some-too-long-query-string-so-that-the-whole-url-is-longer-than-255-characters;some-too-long-query-string-so-that-the-whole-url-is-longer-than-255-characters\n")

rfc3986 = bgroup "rfc3986"
          [ bench "uri" $ whnf (parse uri) testUri]

rfc2616 = bgroup "rfc2616"
          [ bench "request" $ whnf (parse request) parseGetData]