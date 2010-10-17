{-# LANGUAGE 
    OverloadedStrings
  , PackageImports
  , DeriveDataTypeable
  , TupleSections
  #-}

{-

  http://www.ietf.org/rfc/rfc3986.txt

  TODO: implement ipv6 and ipvfuture
-}

module Network.Http.Parser.Rfc3986 where

import Control.Applicative hiding (many)
import Data.Attoparsec as AW
import Data.Attoparsec.Combinator as DACO
import Data.Attoparsec.Char8 as AC hiding (digit, char)
import qualified Data.Attoparsec.Char8 as DAC
import qualified Data.Attoparsec.FastSet as AF
import Data.ByteString as W
import Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8, Word64)
import Data.Char (digitToInt, isAsciiUpper, isAsciiLower)
import Data.List (concat)
import Prelude hiding (take, takeWhile)
import Data.Typeable (Typeable)
import Data.Data (Data)
import qualified Network.Http.Parser.RfcCommon as RC

data URI = URI
    { uriScheme     :: String
    , uriAuthority  :: Maybe URIAuth
    , uriPath       :: String
    , uriQuery      :: String
    , uriFragment   :: String
    } deriving (Eq, Typeable, Data, Show)

data URIAuth = URIAuth
    { uriUserInfo   :: String
    , uriRegName    :: String
    , uriPort       :: String
    } deriving (Eq, Typeable, Data, Show)

nullURI :: URI
nullURI = URI
    { uriScheme     = ""
    , uriAuthority  = Nothing
    , uriPath       = ""
    , uriQuery      = ""
    , uriFragment   = ""
    }

-- Prelude.map ord "!$&'()*+,;="
subDelimsSet = [33,36,38,39,40,41,42,43,44,59,61]
isSubDelims w = AF.memberWord8 w $ AF.fromList subDelimsSet
subDelims :: Parser Word8
subDelims =  AW.satisfy isSubDelims

-- Prelude.map ord ":/?#[]@"
genDelimsSet = [58,47,63,35,91,93,64]
isGenDelims w = AF.memberWord8 w $ AF.fromList genDelimsSet
genDelims :: Parser Word8
genDelims =  AW.satisfy isGenDelims

isReserved w = isGenDelims w || isSubDelims w
reserved :: Parser Word8
reserved = AW.satisfy isReserved

unreserved :: Parser Word8
unreserved = RC.alpha <|> RC.digit <|> AW.satisfy (AW.inClass "-._~")

pctEncoded :: Parser Word8
pctEncoded = cat <$> word8 37 
             <*> AW.satisfy RC.hex_pred
             <*> AW.satisfy RC.hex_pred
  where 
    cat _ b c = toTen b * 16 + toTen c
    toTen w | w >= 48 && w <= 57  =  fromIntegral (w - 48)
            | w >= 97 && w <= 102 =  fromIntegral (w - 87)
            | otherwise           =  fromIntegral (w - 55)
{-# INLINE pctEncoded #-}

uchar extras = unreserved <|> pctEncoded <|> subDelims <|> AW.satisfy (AW.inClass extras)
pchar = uchar ":@"
fragment :: Parser [Word8]
fragment = (35:) <$> many (uchar ":@/?")
query = (63:) <$> many (uchar ":@/?")
segment, segmentNz, segmentNzNc, slashSegment :: Parser [Word8]
segment = many pchar
segmentNz = many1 pchar
segmentNzNc = many1 $ uchar "@"
slashSegment = (:) <$> word8 47 <*> segment
pathRootless = RC.appcon <$> segmentNz <*> many slashSegment
pathNoscheme = RC.appcon <$> segmentNzNc <*> many slashSegment
pathAbsolute = (:) <$> word8 47 <*> option [] pathRootless
pathAbempty = Prelude.concat <$> many slashSegment
regName = many (unreserved <|> pctEncoded <|> subDelims)

decOctet :: Parser [Word8]
decOctet = do
  x <- many RC.digit
  if read (C.unpack . W.pack $ x) > 255 
    then fail "error decOctet"
    else return x

ipv4address :: Parser [Word8]
ipv4address = ret <$> decOctet <* word8 46
                  <*> decOctet <* word8 46
                  <*> decOctet <* word8 46
                  <*> decOctet
  where ret a b c d = a++[46]++b++[46]++c++[46]++d

port = many RC.digit

-- TODO: IP-literal
-- host = ipLiteral <|> ipv4address <|> regName
host = regName <|> ipv4address
userinfo = do
  uu <- many (unreserved <|> pctEncoded <|> subDelims <|> word8 58)
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

scheme = (:) <$> RC.alpha <*> many (RC.alpha <|> RC.digit <|> AW.satisfy (AW.inClass "+-."))

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
  return $ URI { uriScheme = RC.toRepr []
               , uriAuthority = ua
               , uriPath = RC.toRepr up
               , uriQuery = RC.toRepr uq
               , uriFragment = RC.toRepr uf
               }

hierPart :: Parser ((Maybe URIAuth), [Word8])
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
  return $ URI { uriScheme = RC.toRepr us
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
  return $ URI
         { uriScheme = RC.toRepr us
         , uriAuthority = ua
         , uriPath = RC.toRepr up
         , uriQuery = RC.toRepr uq
         , uriFragment = RC.toRepr uf
         }

uriReference = uri <|> relativeRef

-- | Utility

-- instance Show URI where
--     showsPrec _ uri = uriToString defaultUserInfoMap uri

-- defaultUserInfoMap :: String -> String
-- defaultUserInfoMap uinf = user++newpass
--     where
--         (user,pass) = Prelude.break (==':') uinf
--         newpass     = if Prelude.null pass || (pass == "@")
--                                            || (pass == ":@")
--                         then pass
--                         else ":...@"

-- uriToString :: (String -> String) -> URI -> ShowS
-- uriToString userinfomap URI { uriScheme = scheme
--                             , uriAuthority=authority
--                             , uriPath=path
--                             , uriQuery=query
--                             , uriFragment=fragment
--                             } =
--   (scheme++) . (uriAuthToString userinfomap authority)
--                  . (path++) . (query++) . (fragment++)

-- uriAuthToString :: (String->String) -> (Maybe URIAuth) -> ShowS
-- uriAuthToString _           Nothing   = id          -- shows ""
-- uriAuthToString userinfomap
--         (Just URIAuth { uriUserInfo = uinfo
--                       , uriRegName  = regname
--                       , uriPort     = port
--                       } ) =
--     ("//"++) . (if Prelude.null uinfo then id else ((userinfomap uinfo)++))
--              . (regname++)
--              . (":"++)
--              . (port++)