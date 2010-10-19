{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  #-}

module Network.Http.Parser.Rfc2045 where

import Control.Applicative hiding (many)
import Data.Attoparsec as AW
import Data.Attoparsec.Char8 as AC hiding (digit, char)
import qualified Data.Attoparsec.Char8 as DAC
import qualified Data.Attoparsec.FastSet as AF
import Data.ByteString as W
import Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8, Word64)
import Data.Char (digitToInt, chr, ord)
import Prelude hiding (take, takeWhile)
import qualified Network.Http.Parser.Rfc3986 as R3986
import Network.Http.Parser.RfcCommon

mimeVersion :: Parser (Int,Int)
mimeVersion = (,) 
              <$> (stringCI "mime-version" *> DAC.char ':' *> lws *> decimal)
              <*> (DAC.char '.' *> decimal)

-- Prelude.map Data.Char.ord "()<>@,;:\\\"/[]?="
tspecialsSet ::[Word8]
tspecialsSet = [40,41,60,62,64,44,59,58,92,34,47,91,93,63,61]
tspecials_pred w = AF.memberWord8 w (AF.fromList tspecialsSet)
tspecials :: Parser Word8
tspecials = AW.satisfy tspecials_pred

token_pred w = char_pred w && not (ctl_pred w || tspecials_pred w)
token :: Parser [Word8]
token = many1 $ AW.satisfy token_pred

value :: Parser [Word8]
value = token <|> quotedString

attribute = token
parameter = (,) <$> (attribute <* DAC.char '=') <*> value
