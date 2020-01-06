{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.2046
-- Copyright   :  Aycan iRiCAN 2010-2020
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
import           Data.Functor                     (($>), void)
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString
import           Data.Word                        (Word8)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2045           (transportPadding)
import           Network.Parser.Rfc2234           (alphaPred, crlf, digitPred,
                                                   manyNtoM, octet)
import           Network.Parser.Rfc2822           (text)
import           Network.Parser.Rfc5322           (Message (..), fields)
import           Network.Parser.Utils
--------------------------------------------------------------------------------
bcharsnospacePred :: Word8 -> Bool
bcharsnospacePred w
  =  digitPred w
  || alphaPred w
  || inClass "'()+_,./:=?-" w

bcharsnospace :: Parser Word8
bcharsnospace = satisfy bcharsnospacePred

bchars :: Parser Word8
bchars = bcharsnospace <|> satisfy (== 32)

-- TODO: 0*69<bchars> bcharsnospace
boundary :: Parser [Word8]
boundary = manyNtoM 0 69 bchars

-- >>> :set -XOverloadedStrings
-- >>> parseOnly (dashBoundary "hebelup hodolo") "--hebelup hodolo\n"
-- Right ()
dashBoundary :: ByteString -> Parser ()
dashBoundary str = void (word8 45 *> word8 45 *> AC.string str <?> "invalid boundary")

encapsulation :: Parser () -> Parser Message
encapsulation boundaryParser
  = (delimiter boundaryParser >> transportPadding >> crlf) *> bodyPart

delimiter :: Parser () -> Parser ()
delimiter boundaryParser = crlf *> boundaryParser

closeDelimiter :: Parser () -> Parser ()
closeDelimiter boundaryParser = boundaryParser >> AC.string "--" >> return ()

discardText :: Parser Word8
discardText = many line *> text
  where line = option 32 (many text *> crlf)

bodyPart :: Parser Message
bodyPart = Message <$> fields
                   <*> option Nothing (Just . pack <$> (crlf *> many octet))

preamble, epilogue :: Parser Word8
preamble = discardText $> 0
epilogue = discardText $> 0

-- * Multipart Body Parsing

-- | Parse multipart body ( we ignore preamble given before boundary
-- of the message body)
multipartBody :: ByteString -> Parser [Message]
multipartBody str = do
  _   <- manyTill octet begin
  parseTill (snoc' <$> many (parseTill bodyPart sep)
                   <*> bodyPart) end
  where
    begin = void $ dashBoundary str *> transportPadding
    sep   = void $ crlf *> begin *> crlf
    end   = void $ crlf *> closeDelimiter begin
    -- prepend
    snoc' :: [a] -> a -> [a]
    snoc' xs = (xs ++) . (:[])
