{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  #-}

-- | Multipurpose Internet Mail Extensions (MIME) Part Two: Media Types
-- <http://www.ietf.org/rfc/rfc2046.txt>
module Network.Parser.Rfc2046 where

import Control.Monad (join)
import Control.Applicative as A hiding (many)
import Data.Attoparsec
import qualified Data.Attoparsec.Char8 as AC
import Data.ByteString as W
import Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import Network.Parser.RfcCommon hiding (text)
import Network.Parser.Rfc2234
import Network.Parser.Rfc2045
import Network.Parser.Rfc2822
import qualified Data.Map as M
import Prelude hiding (id)

-- Prelude.map Data.Char.ord "'()+_,-./:=?"
-- bcharsnospaceSet' :: [Word8]
-- bcharsnospaceSet' = [39,40,41,43,95,44,45,46,47,58,61,63]
bcharsnospace_pred :: Word8 -> Bool
bcharsnospace_pred w = digit_pred w 
                       || alpha_pred w
                       || inClass "'()+_,-./:=?" w -- F.memberWord8 w (F.fromList bcharsnospaceSet')
bcharsnospace :: Parser Word8
bcharsnospace = satisfy bcharsnospace_pred

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
