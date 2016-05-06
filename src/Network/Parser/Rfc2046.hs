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
import           Debug.Trace
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2045
import           Network.Parser.Rfc2234
import qualified Network.Parser.Rfc5322           as R5322
import           Network.Parser.RfcCommon
--------------------------------------------------------------------------------
-- Prelude.map Data.Char.ord "'()+_,-./:=?"
bcharsnospacePred :: Word8 -> Bool
bcharsnospacePred w
  =  digitPred w
  || alphaPred w
  || inClass "'()+_,./:=?-" w

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

encapsulation :: Parser () -> Parser R5322.Message
encapsulation boundaryParser
  = (delimiter boundaryParser >> transportPadding >> crlf) *> bodyPart

delimiter :: Parser () -> Parser ()
delimiter boundaryParser = crlf *> boundaryParser

closeDelimiter :: Parser () -> Parser ()
closeDelimiter boundaryParser = boundaryParser >> AC.string "--" >> return ()

discardText = many line *> text
  where line = option 32 (many text *> crlf)

bodyPart :: Parser R5322.Message
bodyPart = R5322.message

preamble, epilogue :: Parser Word8
preamble = discardText *> return 0
epilogue = discardText *> return 0

-- >>> c <- Prelude.readFile "test/mime-wiki.txt"
multipartBody :: ByteString -> Parser [R5322.Message]
multipartBody str = do
  let sep = dashBoundary str *> transportPadding
  manyTill octet (sep *> crlf)
  xs <- many (parseTill bodyPart $ sep <* crlf)
  x <- parseTill bodyPart $ sep <* "--"
  return $ xs ++ [x]

-- multipartBody2 ::ByteString -> Parser [R5322.Message]
-- multipartBody2 str = do
--   let sep = dashBoundary str *> transportPadding
--   sep <|> bodyPart
-- mimeBoundary :: Maybe ByteString -> Parser ByteString
-- mimeBoundary Nothing = pack <$> (AC.string "--" *> manyNtoM 0 69 bchars)
-- mimeBoundary (Just s) = AC.string ("--" <> s) <* skipMany (AC.char '-')

-- | delimit this parser to a fixed byte in order to close dos attacks
parseTill :: Parser a -> Parser b -> Parser a
parseTill p end = do
  ds <- pack <$> manyTill octet end
  case parseOnly p ds of
    Left err  -> fail err
    Right ret -> return ret


