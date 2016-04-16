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
import           Data.Word                        (Word8)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2045
import           Network.Parser.Rfc2234
--------------------------------------------------------------------------------
-- Prelude.map Data.Char.ord "'()+_,-./:=?"
-- bcharsnospaceSet' :: [Word8]
-- bcharsnospaceSet' = [39,40,41,43,95,44,45,46,47,58,61,63]
bcharsnospacePred :: Word8 -> Bool
bcharsnospacePred w = digitPred w
                       || alphaPred w
                       || inClass "'()+_,-./:=?" w -- F.memberWord8 w (F.fromList bcharsnospaceSet')
bcharsnospace :: Parser Word8
bcharsnospace = satisfy bcharsnospacePred

bchars = bcharsnospace <|> satisfy (== 32)

-- TODO: 0*69<bchars> bcharsnospace
boundary = many1 bchars
dashBoundary = word8 45 *> word8 45 *> boundary

encapsulation = ret <$> delimiter <* transportPadding
                <*> crlf *> bodyPart
    where ret d b = (d,b)

delimiter = crlf *> dashBoundary

-- discardText = many line *> text
--   where line = option 32 (many text *> crlf)

bodyPart = ret <$> mimePartHeaders
           <*> option [] (crlf *> many' octet)
    where ret hs d = (hs,d)
