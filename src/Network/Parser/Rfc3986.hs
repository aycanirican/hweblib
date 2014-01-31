{-# LANGUAGE 
    OverloadedStrings
  , TupleSections
  #-}

-- | Uniform Resource Identifier (URI): Generic Syntax
-- <http://www.ietf.org/rfc/rfc3986.txt>
module Network.Parser.Rfc3986 where

--  TODO: implement ipv6 and ipvfuture
import Control.Applicative hiding (many)
import Data.Attoparsec
--import Data.Attoparsec.Char8
import qualified Data.Attoparsec.Char8 as DAC
import Data.ByteString as W
import Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8, Word64)
import Data.Char (digitToInt, isAsciiUpper, isAsciiLower)
import Data.List (concat)
import Prelude hiding (take, takeWhile)
import Data.Typeable (Typeable)
import qualified Network.Parser.RfcCommon as RC
import Network.Parser.Rfc2234
import Network.Types

-- Prelude.map ord "!$&'()*+,;="
-- subDelimsSet = [33,36,38,39,40,41,42,43,44,59,61]
isSubDelims = inClass "!$&'()*+,;=" -- AF.memberWord8 w $ AF.fromList subDelimsSet
subDelims :: Parser Word8
subDelims =  satisfy isSubDelims

-- Prelude.map ord ":/?#[]@"
-- genDelimsSet = [58,47,63,35,91,93,64]
isGenDelims = inClass ":/?#[]@" -- AF.memberWord8 w $ AF.fromList genDelimsSet
genDelims :: Parser Word8
genDelims =  satisfy isGenDelims

isReserved w = isGenDelims w || isSubDelims w
reserved :: Parser Word8
reserved = satisfy isReserved

unreserved :: Parser Word8
unreserved = alpha <|> digit <|> satisfy (inClass "-._~")

pctEncoded :: Parser Word8
pctEncoded = cat <$> word8 37
             <*> satisfy hexdig_pred
             <*> satisfy hexdig_pred
  where 
    cat _ b c = toTen b * 16 + toTen c
    toTen w | w >= 48 && w <= 57  =  fromIntegral (w - 48)
            | w >= 97 && w <= 102 =  fromIntegral (w - 87)
            | otherwise           =  fromIntegral (w - 55)
{-# INLINE pctEncoded #-}

uchar extras = unreserved <|> pctEncoded <|> subDelims <|> satisfy (inClass extras)
pchar = uchar ":@"
fragment :: Parser [Word8]
fragment = (35:) <$> many' (uchar ":@/?")
query = (63:) <$> many' (uchar ":@/?")
segment, segmentNz, segmentNzNc, slashSegment :: Parser [Word8]
segment = many' pchar
segmentNz = many1 pchar
segmentNzNc = many1 $ uchar "@"
slashSegment = (:) <$> word8 47 <*> segment
pathRootless = RC.appcon <$> segmentNz <*> many' slashSegment
pathNoscheme = RC.appcon <$> segmentNzNc <*> many' slashSegment
pathAbsolute = (:) <$> word8 47 <*> option [] pathRootless
pathAbempty = Prelude.concat <$> many' slashSegment
regName = many' (unreserved <|> pctEncoded <|> subDelims)

decOctet :: Parser [Word8]
decOctet = do
  x <- many' digit
  if read (C.unpack . W.pack $ x) > 255 
    then fail "error decOctet"
    else return x

ipv4address :: Parser [Word8]
ipv4address = ret <$> decOctet <* word8 46
                  <*> decOctet <* word8 46
                  <*> decOctet <* word8 46
                  <*> decOctet
  where ret a b c d = a++[46]++b++[46]++c++[46]++d

port = many' digit

-- TODO: IP-literal
-- host = ipLiteral <|> ipv4address <|> regName
host = regName <|> ipv4address
userinfo = do
  uu <- many' (unreserved <|> pctEncoded <|> subDelims <|> word8 58)
  word8 64
  return uu

authority :: Parser (Maybe URIAuth)
authority = do
  uu <- option [] (try userinfo)
  uh <- host
  up <- option [] (word8 58 *> port)
  return . Just $ URIAuth
            { uriUserInfo = C.unpack $ W.pack uu
            , uriRegName  = C.unpack $ W.pack uh
            , uriPort     = C.unpack $ W.pack up
            }

scheme = (:) <$> alpha <*> many' (alpha <|> digit <|> satisfy (inClass "+-."))

relativePart = do try (word8 47 *> word8 47)
                  uu <- option Nothing authority
                  pa <- pathAbempty
                  return (uu,pa) 
          <|> ((Nothing,) <$> pathAbsolute)
          <|> ((Nothing,) <$> pathNoscheme)
          <|> pure (Nothing, [])

relativeRef = do
  (ua,up) <- relativePart
  uq <- option [] (word8 63 *> query)
  uf <- option [] (word8 35 *> fragment)
  return URI { uriScheme = RC.toRepr []
             , uriAuthority = ua
             , uriPath = RC.toRepr up
             , uriQuery = RC.toRepr uq
             , uriFragment = RC.toRepr uf
             }

hierPart :: Parser (Maybe URIAuth, [Word8])
hierPart = do try (word8 47 *> word8 47)
              uu <- option Nothing authority
              pa <- pathAbempty
              return (uu,pa)
       <|> ((Nothing,) <$> pathAbsolute)
       <|> ((Nothing,) <$> pathRootless)
       <|> pure (Nothing, [])

absoluteUri :: Parser URI
absoluteUri = do
  us <- scheme
  word8 58
  (ua,up) <- hierPart
  uq <- option [] (word8 63 *> query)
  return URI { uriScheme = RC.toRepr us
             , uriAuthority = ua
             , uriPath = RC.toRepr up
             , uriQuery = RC.toRepr uq
             , uriFragment = RC.toRepr []
             }

uri = do
  us <- scheme
  word8 58
  (ua,up) <- hierPart
  uq <- option [] (word8 63 *> query)
  uf <- option [] (word8 35 *> fragment)
  return URI
         { uriScheme = RC.toRepr us
         , uriAuthority = ua
         , uriPath = RC.toRepr up
         , uriQuery = RC.toRepr uq
         , uriFragment = RC.toRepr uf
         }

uriReference = uri <|> relativeRef
