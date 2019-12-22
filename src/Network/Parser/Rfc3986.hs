{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
-- |
-- Module      :  Network.Parser.3986
-- Copyright   :  Aycan iRiCAN 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Uniform Resource Identifier (URI): Generic Syntax
-- <http://www.ietf.org/rfc/rfc3986.txt>
--  TODO: implement ipv6 and ipvfuture

module Network.Parser.Rfc3986 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString
import qualified Data.ByteString.Char8            as BSC
import           Data.Monoid                      (mempty, (<>))
import           Data.Word                        (Word8)
import           Prelude                          hiding (take, takeWhile)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2234
import           Network.Types
--------------------------------------------------------------------------------

subDelims :: Parser Word8
subDelims =  satisfy $ inClass "!$&'()*+,;="

genDelims :: Parser Word8
genDelims =  satisfy $ inClass ":/?#[]@"

reserved :: Parser Word8
reserved = genDelims <|> subDelims

unreserved :: Parser Word8
unreserved = alpha <|> digit <|> satisfy (inClass "-._~")

pctEncoded :: Parser Word8
pctEncoded
  = cat <$> AC.char '%'
        <*> satisfy hexdigPred
        <*> satisfy hexdigPred
  where
    cat _ b c = toTen b * 16 + toTen c
    toTen w | w >= 48 && w <= 57  =  fromIntegral (w - 48)
            | w >= 97 && w <= 102 =  fromIntegral (w - 87)
            | otherwise           =  fromIntegral (w - 55)
{-# INLINABLE pctEncoded #-}

uchar :: String -> Parser Word8
uchar extras = unreserved <|> pctEncoded <|> subDelims <|> satisfy (inClass extras)

pchar :: Parser Word8
pchar = uchar ":@"

fragment :: Parser ByteString
fragment = query

-- >>> parse query "foo=bar&zoo=yoo "
-- Done " " "foo=bar&zoo=yoo"
query :: Parser ByteString
query = pack <$> many (uchar ":@/?")

segment, segmentNz, segmentNzNc, slashSegment :: Parser [Word8]
segment = many pchar
segmentNz = many1 pchar
segmentNzNc = many1 $ uchar "@"
slashSegment = (:) <$> word8 47 <*> segment

-- >> parse pathRootless "foo/bar/baz "
-- Done " " "foo/bar/baz"
pathRootless :: Parser ByteString
pathRootless = (\a b -> pack (a ++ join b)) <$> segmentNz <*> many slashSegment

pathNoscheme :: Parser ByteString
pathNoscheme = (\a b -> pack (a ++ join b)) <$> segmentNzNc <*> many slashSegment

pathAbsolute :: Parser ByteString
pathAbsolute = ("/" <>) <$> (word8 47 *> option mempty pathRootless)

pathAbempty :: Parser ByteString
pathAbempty = pack . join <$> many slashSegment

regName :: Parser ByteString
regName = pack <$> many (unreserved <|> pctEncoded <|> subDelims)

decOctet :: Parser ByteString
decOctet = pack . ret <$> many1 digit
  where
    ret :: [Word8] -> [Word8]
    ret x = if read (BSC.unpack . pack $ x) > (255 :: Int)
            then fail "wrong decOctet"
            else x


ipv4address :: Parser ByteString
ipv4address = fst <$> match (decOctet >> "."
                             >> decOctet >> "."
                             >> decOctet >> "."
                             >> decOctet)

port :: Parser ByteString
port = pack <$> many digit

-- TODO: IP-literal
-- host = ipLiteral <|> ipv4address <|> regName

host :: Parser ByteString
host = regName <|> ipv4address

userinfo :: Parser ByteString
userinfo = pack <$> many (unreserved <|> pctEncoded <|> subDelims <|> word8 58) <* word8 64

authority :: Parser (Maybe URIAuth)
authority = do
  uu <- option mempty userinfo
  uh <- host
  up <- option mempty (word8 58 *> port)
  return . Just $ URIAuth
            { uriUserInfo = uu
            , uriRegName  = uh
            , uriPort     = up
            }

scheme :: Parser ByteString
scheme = (\x xs -> pack (x:xs)) <$> alpha
                                <*> many (alpha <|> digit <|> satisfy (inClass "+.-"))

relativePart :: Parser (Maybe URIAuth, ByteString)
relativePart = ((,) <$> (word8 47 *> word8 47 *> option Nothing authority)
                    <*> pathAbempty)
               <|> ((Nothing,) <$> pathAbsolute)
               <|> ((Nothing,) <$> pathNoscheme)
               <|> pure (Nothing, mempty)

relativeRef :: Parser URI
relativeRef = do
  (ua,up) <- relativePart
  uq <- option mempty (word8 63 *> query)
  uf <- option mempty (word8 35 *> fragment)
  return URI { uriScheme = mempty
             , uriAuthority = ua
             , uriPath = up
             , uriQuery = uq
             , uriFragment = uf
             }

hierPart :: Parser (Maybe URIAuth, ByteString)
hierPart = do _  <- try (word8 47 *> word8 47)
              uu <- option Nothing authority
              pa <- pathAbempty
              return (uu,pa)
       <|> ((Nothing,) <$> pathAbsolute)
       <|> ((Nothing,) <$> pathRootless)
       <|> pure (Nothing, mempty)

absoluteUri :: Parser URI
absoluteUri = do
  us <- scheme
  _ <- word8 58
  (ua,up) <- hierPart
  uq <- option mempty (word8 63 *> query)
  return URI { uriScheme = us
             , uriAuthority = ua
             , uriPath = up
             , uriQuery = uq
             , uriFragment = mempty
             }

uri :: Parser URI
uri = do
  us <- scheme
  _ <- word8 58
  (ua,up) <- hierPart
  uq <- option mempty (word8 63 *> query)
  uf <- option mempty (word8 35 *> fragment)
  return URI
         { uriScheme = us
         , uriAuthority = ua
         , uriPath = up
         , uriQuery = uq
         , uriFragment = uf
         }

uriReference :: Parser URI
uriReference = uri <|> relativeRef
