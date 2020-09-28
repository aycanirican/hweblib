{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Module      :  Network.Parser.7234
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Hypertext Transfer Protocol (HTTP/1.1): Caching
--
-- <http://www.ietf.org/rfc/rfc7234.txt>

module Network.Parser.Rfc7234 where

import Control.Applicative (optional, Alternative ((<|>)))
import Data.Attoparsec.ByteString as A
  ( Parser,
    many',
    many1,
    match,
    try,
  )
import Data.Attoparsec.ByteString.Char8 as AC
  ( char,
    digit,
    string,
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Time (UTCTime (..))
import Data.Typeable (Proxy (..), Typeable)
import qualified GHC.Generics as GHC
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.Parser.Rfc5234 as R5234
import Network.Parser.Rfc7230 (uriHost, quotedString, dash1, port, pseudonym, token)
import Network.Parser.Rfc7231 (httpDate)
import Data.Maybe (fromMaybe)
data CacheDirective
  = MaxAge Integer
  | MinStale Integer
  | MinFresh Integer
  | NoCache [ByteString]
  | NoStore
  | NoTransform
  | OnlyIfCached
  | MustRevalidate
  | Public
  | Private [ByteString]
  | ProxyRevalidate
  | SMaxAge Integer
  | CacheDirectiveExtension ByteString (Maybe ByteString)
    deriving (Eq, Show, Typeable, GHC.Generic)
-- * 1.2 Syntax Notation

-- * 1.2.1.  Delta Seconds
deltaSeconds :: Parser Integer
deltaSeconds = read <$> many1 digit

-- * 5.  Header Field Definitions
-- * 5.1.  Age
age :: Parser Integer
age = deltaSeconds

-- * 5.2.  Cache-Control

-- |
-- >>> Data.Attoparsec.parseOnly cacheControl "private, community=\"UCI\""
-- Right [Private [],CacheDirectiveExtension "community" Nothing]
cacheControl :: Parser [CacheDirective]
cacheControl = dash1 cacheDirective

cacheDirective :: Parser CacheDirective
cacheDirective
  =     MaxAge
          <$> toParser (Proxy :: Proxy (CacheDirective' "max-age" (RequiredParam Integer)))
    <|> MinStale
          <$> toParser (Proxy :: Proxy (CacheDirective' "max-stale" (RequiredParam Integer)))
    <|> MinFresh
          <$> toParser (Proxy :: Proxy (CacheDirective' "min-fresh" (RequiredParam Integer)))
    <|> NoCache . fromMaybe []
          <$> toParser (Proxy :: Proxy (CacheDirective' "no-cache" (OptionalParam [ByteString])))
    <|> NoStore
           <$ toParser (Proxy :: Proxy (CacheDirective' "no-store" NoParam))
    <|> NoTransform
           <$ toParser (Proxy :: Proxy (CacheDirective' "no-transform" NoParam))
    <|> OnlyIfCached
           <$ toParser (Proxy :: Proxy (CacheDirective' "only-if-cached" NoParam))
    <|> MustRevalidate
           <$ toParser (Proxy :: Proxy (CacheDirective' "must-revalidate" NoParam))
    <|> Public
           <$ toParser (Proxy :: Proxy (CacheDirective' "public" NoParam))
    <|> Private . fromMaybe []
          <$> toParser (Proxy :: Proxy (CacheDirective' "private" (OptionalParam [ByteString])))
    <|> ProxyRevalidate
           <$ toParser (Proxy :: Proxy (CacheDirective' "proxy-revalidate" NoParam))
    <|> SMaxAge
          <$> toParser (Proxy :: Proxy (CacheDirective' "s-max-age" (RequiredParam Integer)))
    <|> CacheDirectiveExtension
          <$> token <*> optional (AC.char '=' *> token)

--------------------------------------------------------------------------------

class HasParser a where
  type ParseResult a
  toParser :: Proxy a -> Parser (ParseResult a)

data CacheDirective' (a :: Symbol) (b :: *)

instance (KnownSymbol a, HasParamParser b) => HasParser (CacheDirective' a b) where
  type ParseResult (CacheDirective' a b) = ParamParseResult b
  toParser (Proxy :: Proxy (CacheDirective' a b))
    = AC.string (BC.pack k) *> toParamParser (Proxy :: Proxy b)
    where k = symbolVal (Proxy :: Proxy a)

instance HasParser Integer where
  type ParseResult Integer = Integer
  toParser _ = read <$> many' digit

instance HasParser ByteString where
  type ParseResult ByteString = ByteString
  toParser _ = token

instance HasParser a => HasParser [a] where
  type ParseResult [a] = [ParseResult a]
  toParser (Proxy :: Proxy [a]) = dash1 $ toParser (Proxy :: Proxy a)

--------------------------------------------------------------------------------

class HasParamParser a where
  type ParamParseResult a
  toParamParser :: Proxy a -> Parser (ParamParseResult a)

data OptionalParam a
data RequiredParam a
data NoParam

instance HasParamParser NoParam where
  type ParamParseResult NoParam = ()
  toParamParser _ = return ()

instance HasParser a => HasParamParser (RequiredParam a) where
  type ParamParseResult (RequiredParam a) = ParseResult a
  toParamParser (Proxy :: Proxy (RequiredParam a))
    = AC.char '=' *> toParser (Proxy :: Proxy a)

instance HasParser a => HasParamParser (OptionalParam a) where
  type ParamParseResult (OptionalParam a) = Maybe (ParseResult a)
  toParamParser (Proxy :: Proxy (OptionalParam a))
    = optional (AC.char '=' *> toParser (Proxy :: Proxy a))

-------------------------------------------------------------------------------

-- * 5.3.  Expires

expires :: Parser UTCTime
expires = httpDate

-- * 5.4.  Pragma

{-
Pragma           = 1#pragma-directive
pragma-directive = "no-cache" / extension-pragma
extension-pragma = token [ "=" ( token / quoted-string ) ]
-}

data PragmaDirective
  = PragmaNoCache
  | PragmaExtension ByteString (Maybe ByteString)

pragma :: Parser [PragmaDirective]
pragma = dash1 pragmaDirective

pragmaDirective :: Parser PragmaDirective
pragmaDirective
    = PragmaNoCache <$ "no-cache"
  <|> PragmaExtension
        <$> token
        <*> optional (token <|> quotedString)

-- * 5.5.  Warning

{-
Warning       = 1#warning-value

warning-value = warn-code SP warn-agent SP warn-text
                                      [ SP warn-date ]

warn-code  = 3DIGIT
warn-agent = ( uri-host [ ":" port ] ) / pseudonym
                ; the name or pseudonym of the server adding
                ; the Warning header field, for use in debugging
                ; a single "-" is recommended when agent unknown
warn-text  = quoted-string
warn-date  = DQUOTE HTTP-date DQUOTE
-}

-- |
-- >>> Data.Attoparsec.parseOnly warning "112 - \"network down\" \"Sat, 25 Aug 2012 23:34:45 GMT\""
-- Right [(112,"-","network down",Just 2012-08-25 23:34:45 UTC)]
-- >>> Data.Attoparsec.parseOnly warning "113 - \"Heuristic Expiration\""
-- Right [(113,"-","Heuristic Expiration",Nothing)]
warning :: Parser [(Integer, ByteString, ByteString, Maybe UTCTime)]
warning = dash1 warningValue

warningValue :: Parser (Integer, ByteString, ByteString, Maybe UTCTime)
warningValue = (,,,) <$> warnCode
                     <*> (R5234.sp *> warnAgent)
                     <*> (R5234.sp *> warnText)
                     <*> optional (R5234.sp *> warnDate)

warnCode :: Parser Integer
warnCode = read <$> R5234.manyN 3 digit

warnAgent :: Parser ByteString
warnAgent = snd <$> match (uriHost >> try (AC.char '*' *> port))
         <|> pseudonym

warnText :: Parser ByteString
warnText = quotedString

warnDate :: Parser UTCTime
warnDate = R5234.dquote *> httpDate <* R5234.dquote

-- * 7.1.3.  Registrations

{-
TODO:
stale-if-error         | [RFC5861], Section 4
stale-while-revalidate | [RFC5861], Section 3
-}

