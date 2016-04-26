{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.2046
-- Copyright   :  Aycan iRiCAN 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Multipurpose Internet Mail Extensions (MIME) Part Two: Media Types
-- <http://www.ietf.org/rfc/rfc2046.txt>

module Network.Parser.Rfc2046 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad                    (join)
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.Attoparsec.Combinator
import           Data.ByteString
import qualified Data.ByteString.Char8            as BSC
import           Data.Monoid
import           Data.Word                        (Word8)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2045
import           Network.Parser.Rfc2234
import           Network.Parser.RfcCommon
--------------------------------------------------------------------------------
-- Prelude.map Data.Char.ord "'()+_,-./:=?"
bcharsnospacePred :: Word8 -> Bool
bcharsnospacePred w
  =  digitPred w
  || alphaPred w
  || inClass "'()+_,-./:=?" w

bcharsnospace :: Parser Word8
bcharsnospace = satisfy bcharsnospacePred

bchars = bcharsnospace <|> satisfy (== 32)

-- TODO: 0*69<bchars> bcharsnospace
boundary :: Parser [Word8]
boundary = manyNtoM 0 69 bchars

-- >>> parseOnly (dashBoundary "hebeluphodolo") "--hebeluphodolo"
-- Right ()
dashBoundary :: ByteString -> Parser ()
dashBoundary str = (word8 45 *> word8 45 *> AC.string str <?> "invalid boundary") >> return ()

encapsulation :: Parser () -> Parser ([Header], [Word8])
encapsulation boundaryParser
  = (delimiter boundaryParser >> transportPadding >> crlf) *> bodyPart boundaryParser

delimiter :: Parser () -> Parser ()
delimiter boundaryParser = crlf *> boundaryParser

closeDelimiter :: Parser () -> Parser ()
closeDelimiter boundaryParser = boundaryParser >> AC.string "--" >> return ()

discardText = many line *> text
  where line = option 32 (many text *> crlf)

bodyPart :: Parser ()
         -> Parser ([Header], [Word8])
bodyPart boundaryParser
  = ret <$> (mimePartHeaders <?> "invalid headers")
        <*> option [] (crlf *> manyTill octet boundaryParser)
    where ret hs d = (hs,d)

preamble, epilogue :: Parser Word8
preamble = discardText *> return 0
epilogue = discardText *> return 0

-- >>> c <- Prelude.readFile "test/mime-wiki.txt"
multipartBody :: ByteString -> Parser ([Header], [Word8])
multipartBody boundary = do
  let boundaryParser = dashBoundary boundary <* transportPadding
--   option 0 (preamble <* crlf)
  body <- boundaryParser *> (bodyPart boundaryParser <?> "invalid bodyPart") -- <* many (encapsulation boundaryParser)
  closeDelimiter boundaryParser >> transportPadding
--  option 0 (crlf *> epilogue)
  return body

-- mimeBoundary :: Maybe ByteString -> Parser ByteString
-- mimeBoundary Nothing = pack <$> (AC.string "--" *> manyNtoM 0 69 bchars)
-- mimeBoundary (Just s) = AC.string ("--" <> s) <* skipMany (AC.char '-')

