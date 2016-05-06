{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}


-- |
-- Module      :  Network.Parser.7234
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2015
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
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Attoparsec.ByteString       as A
import           Data.Attoparsec.ByteString.Char8 as AC
import           Data.Attoparsec.Combinator
import           Data.ByteString
import qualified Data.ByteString.Char8            as BC
import           Data.Char                        (digitToInt)
import           Data.Functor
import           Data.Monoid
import           Data.Scientific
import           Data.Time
import           Data.Typeable
import qualified GHC.Generics                     as GHC
import           GHC.TypeLits
import           Prelude                          hiding (product)
--------------------------------------------------------------------------------
import qualified Network.Parser.Rfc5234           as R5234
import           Network.Parser.Rfc7230
import           Network.Parser.Rfc7231           (http_date)
-- import           Network.Parser.Rfc7232           (entity_tag)
--------------------------------------------------------------------------------

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
    deriving (Eq,Show,Typeable,GHC.Generic)
-- * 1.2 Syntax Notation

-- * 1.2.1.  Delta Seconds
delta_seconds :: Parser Integer
delta_seconds = read <$> many1 digit

-- * 5.  Header Field Definitions
-- * 5.1.  Age
age :: Parser Integer
age = delta_seconds

-- * 5.2.  Cache-Control

-- |
-- >>> parseOnly cache_control "private, community=\"UCI\""
-- Right [Private [],CacheDirectiveExtension "community" Nothing]
cache_control :: Parser [CacheDirective]
cache_control = dash1 cache_directive

cache_directive :: Parser CacheDirective
cache_directive
  =     MaxAge
          <$> toParser (Proxy :: Proxy (CacheDirective' "max-age" (RequiredParam Integer)))
    <|> MinStale
          <$> toParser (Proxy :: Proxy (CacheDirective' "max-stale" (RequiredParam Integer)))
    <|> MinFresh
          <$> toParser (Proxy :: Proxy (CacheDirective' "min-fresh" (RequiredParam Integer)))
    <|> NoCache . maybe [] id
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
    <|> Private . maybe [] id
          <$> toParser (Proxy :: Proxy (CacheDirective' "private" (OptionalParam [ByteString])))
    <|> ProxyRevalidate
           <$ toParser (Proxy :: Proxy (CacheDirective' "proxy-revalidate" NoParam))
    <|> SMaxAge
          <$> toParser (Proxy :: Proxy (CacheDirective' "s-max-age" (RequiredParam Integer)))
    <|> CacheDirectiveExtension
          <$> token <*> option Nothing (Just <$> (AC.char '=' *> token))

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
    = option Nothing (Just <$> (AC.char '=' *> toParser (Proxy :: Proxy a)))

-------------------------------------------------------------------------------

-- * 5.3.  Expires

expires :: Parser UTCTime
expires = http_date

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
pragma = dash1 pragma_directive

pragma_directive :: Parser PragmaDirective
pragma_directive
    = PragmaNoCache <$ "no-cache"
  <|> PragmaExtension
        <$> token
        <*> option Nothing (Just <$> (token <|> quoted_string))

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
-- >>> parseOnly warning "112 - \"network down\" \"Sat, 25 Aug 2012 23:34:45 GMT\""
-- Right [(112,"-","network down",Just 2012-08-25 23:34:45 UTC)]
-- >>> parseOnly warning "113 - \"Heuristic Expiration\""
-- Right [(113,"-","Heuristic Expiration",Nothing)]
warning :: Parser [(Integer, ByteString, ByteString, Maybe UTCTime)]
warning = dash1 warning_value

warning_value :: Parser (Integer, ByteString, ByteString, Maybe UTCTime)
warning_value = (,,,) <$> warn_code
                      <*> (R5234.sp *> warn_agent)
                      <*> (R5234.sp *> warn_text)
                      <*> (option Nothing $ Just <$> (R5234.sp *> warn_date))

warn_code :: Parser Integer
warn_code = read <$> R5234.manyN 3 digit

warn_agent :: Parser ByteString
warn_agent = snd <$> match (uri_host >> try (AC.char '*' *> port))
         <|> pseudonym

warn_text :: Parser ByteString
warn_text = quoted_string

warn_date :: Parser UTCTime
warn_date = R5234.dquote *> http_date <* R5234.dquote

-- * 7.1.3.  Registrations

{-
TODO:
stale-if-error         | [RFC5861], Section 4
stale-while-revalidate | [RFC5861], Section 3
-}

