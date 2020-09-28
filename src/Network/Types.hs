{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Network.Types
       ( -- * HTTP Types
         HTTPVersion(..)
       , http10
       , http11
       , http20
       , Headers
       , RequestUri(..)
       , URI(..)
       , URIAuth(..)

         -- * Misc Types
       , HttpError(..)

       ) where

import Control.Exception (Exception)
import Data.ByteString.Char8 (ByteString)
import Data.Typeable (Typeable)
import qualified GHC.Generics as GHC

-- | HTTP Version holds major and minor numbers.
data HTTPVersion =
  HTTPVersion { httpMajor :: Int
              , httpMinor :: Int
              } deriving (Eq, Show, GHC.Generic)

-- | HTTP 1.0
http10 :: HTTPVersion
http10 = HTTPVersion 1 0

-- | HTTP 1.1
http11 :: HTTPVersion
http11 = HTTPVersion 1 1

-- | HTTP 2.0
http20 :: HTTPVersion
http20 = HTTPVersion 2 0

-- data HttpMessage
--   = HttpMessage { mVersion        :: HTTPVersion
--                 , mGeneralHeaders :: GeneralHeaders
--                 , mUnknownHeaders :: Int -- TODO: fix this type
--                 , mEntities       :: Int
--                 } deriving (Eq, Show)

-- | HTTP Headers
type Headers = [(ByteString, ByteString)]

-- data Headers
--   = Headers { entityHeaders  :: EntityHeaders
--             , requestHeaders :: RequestHeaders
--             , generalHeaders :: GeneralHeaders
--             } deriving (Eq, Show)
-- data EH = Allow ByteString
--         | Expires UTCTime
--         | ContentLength Int
--         | ContentLanguage ByteString
--           deriving (Eq, Show, GHC.Generic)

-- data EntityHeaders
--   =  EntityHeaders { ehAllow           :: Maybe ByteString
--                    , ehContentEncoding :: Maybe ByteString
--                    , ehContentLanguage :: Maybe ByteString
--                    , ehContentLength   :: Maybe Int
--                    , ehContentLocation :: Maybe ByteString
--                    , ehContentMD5      :: Maybe ByteString
--                    , ehContentRange    :: Maybe ByteString
--                    , ehContentType     :: Maybe ByteString
--                    , ehExpires         :: Maybe UTCTime
--                    , ehLastModified    :: Maybe UTCTime
--                    } deriving (Eq, Show)

-- data RequestHeaders
--   = RequestHeaders { rhAccept             :: Maybe ByteString
--                    , rhAcceptCharset      :: Maybe ByteString
--                    , rhAcceptEncoding     :: Maybe ByteString
--                    , rhAcceptLanguage     :: Maybe ByteString
--                    , rhAuthorization      :: Maybe ByteString
--                    , rhExpect             :: Maybe ByteString
--                    , rhFrom               :: Maybe ByteString
--                    , rhHost               :: Maybe ByteString
--                    , rhIfMatch            :: Maybe ByteString
--                    , rhIfModifiedSince    :: Maybe UTCTime
--                    , rhIfNoneMatch        :: Maybe ByteString
--                    , rhIfRange            :: Maybe ByteString
--                    , rhIfUnmodifiedSince  :: Maybe UTCTime
--                    , rhMaxForwards        :: Maybe ByteString
--                    , rhProxyAuthorization :: Maybe ByteString
--                    , rhRange              :: Maybe ByteString
--                    , rhReferrer           :: Maybe ByteString
--                    , rhTe                 :: Maybe ByteString
--                    , rhUserAgent          :: Maybe ByteString
--                    , rhCookie             :: Maybe ByteString
--                    } deriving (Eq, Show)

-- data GeneralHeaders
--   = GeneralHeaders { ghCacheControl     :: Maybe ByteString
--                    , ghConnection       :: Maybe ByteString
--                    , ghDate             :: Maybe UTCTime
--                    , ghPragma           :: Maybe ByteString
--                    , ghTrailer          :: Maybe ByteString
--                    , ghTransferEncoding :: Maybe ByteString
--                    , ghUpgrade          :: Maybe ByteString
--                    , ghVia              :: Maybe ByteString
--                    , ghWarning          :: Maybe ByteString
--                    }  deriving (Eq, Show)

-- -- | HTTP Methods
-- data Method = GET
--             | HEAD
--             | POST
--             | PUT
--             | DELETE
--             | TRACE
--             | OPTIONS
--             | CONNECT
--             | EXTENSIONMETHOD ByteString
--               deriving (Eq, Show)

-- data Request
--   = Request { rqMethod  :: Method                     -- ^ Request Method
--             , rqUri     :: RequestUri                 -- ^ Request URI
--             , rqVersion :: HTTPVersion                -- ^ HTTP Version as a tuple
--             , rqHeaders :: Headers                    -- ^ HTTP Message Headers
--             , rqBody    :: ByteString                 -- ^ Request Body
--             } deriving (Eq, Show)

data RequestUri
  = Asterisk                  -- ^ like in OPTIONS * HTTP/1.1
  | AbsoluteUri URI           -- ^ commonly used in proxy servers
  | AbsolutePath ByteString   -- ^ like /asd.cgi
  | RelativeRef URI           -- ^ with a query part like /asd.cgi?foo=bar
  | Authority (Maybe URIAuth) -- ^ Just the authority part
    deriving (Eq, Show)

-- data Response =
--   Response {
--       rpCode    :: Int                        -- ^ Response Code
--     , rpHeaders :: Headers                    -- ^ Response Headers
--     , rpVersion :: HTTPVersion                -- ^ HTTP Version
--     , rpMessage :: ByteString                 -- ^ Response Message
--   } deriving (Eq, Show)

data URI = URI
    { uriScheme    :: ByteString    -- ^ Ex: http or https
    , uriAuthority :: Maybe URIAuth -- ^ authority = [ userinfo "@" ] host [ ":" port ]
    , uriPath      :: ByteString    -- ^ Path is the part between the
                                    -- authority and the query
    , uriQuery     :: ByteString    -- ^ Query begins with '?'
    , uriFragment  :: ByteString    -- ^ Fragment begins with '#'
    } deriving (Eq, Show)

data URIAuth = URIAuth
    { uriUserInfo :: ByteString  -- ^ username:password
    , uriRegName  :: ByteString  -- ^ registered name, ex: www.core.gen.tr
    , uriPort     :: ByteString  -- ^ Port as a string
    } deriving (Eq, Show)

-- | HTTP error.
newtype HttpError
    = InvalidRequestError { httpErrorMessage :: String }
      deriving (Eq, Typeable)

instance Exception HttpError

instance Show HttpError where
  show (InvalidRequestError msg) = "Invalid HTTP request: " ++ msg

