{-# LANGUAGE DeriveDataTypeable #-}

module Network.Types 
       ( -- * HTTP Types
         Method(..)
       , HttpVersion(..)
       , http10
       , http11
       , Header(..)
       , RequestUri(..)
       , Request(..)
       , Response(..)
       , URI(..)
       , URIAuth(..)
         
         -- * Misc Types
       , HttpError(..)
         
       ) where

import Control.Exception as Ex
import Data.Typeable
import Data.ByteString.Char8 as C

-- | HTTP Version holds major and minor numbers.
data HttpVersion = 
  HttpVersion { httpMajor :: Int
              , httpMinor :: Int 
              } deriving (Eq, Show)

-- | HTTP 1.0
http10 :: HttpVersion
http10 = HttpVersion 1 0

-- | HTTP 1.1
http11 :: HttpVersion
http11 = HttpVersion 1 1

-- data HttpMessage = Request | Response

-- | HTTP Headers
data Header = GeneralHeader | RequestHeader | EntityHeader

-- | HTTP Methods
data Method = GET 
            | HEAD 
            | POST 
            | PUT 
            | DELETE 
            | TRACE 
            | OPTIONS 
            | CONNECT 
            | EXTENSIONMETHOD ByteString
              deriving (Eq, Show)

data Request =
  Request {
      rqMethod  :: Method                     -- ^ Request Method
    , rqUri     :: RequestUri                 -- ^ Request URI
    , rqVersion :: HttpVersion                -- ^ HTTP Version as a tuple
    , rqHeaders :: [(ByteString, ByteString)] -- ^ Request Headers as an alist
    , rqBody    :: ByteString                 -- ^ Request Body
    } deriving (Eq, Show)

data RequestUri = Asterisk                  -- ^ like in OPTIONS * HTTP/1.1
                | AbsoluteUri URI           -- ^ commonly used in proxy servers
                | AbsolutePath ByteString   -- ^ like /asd.cgi
                | RelativeRef URI           -- ^ with a query part like /asd.cgi?foo=bar
                | Authority (Maybe URIAuth) -- ^ Just the authority part
                deriving (Eq, Show)

data Response = 
  Response {
      rpCode    :: Int                        -- ^ Response Code
    , rpHeaders :: [(ByteString, ByteString)] -- ^ Response Headers as an alist
    , rpVersion :: (Int,Int)                  -- ^ HTTP Version
    , rpMessage :: ByteString                 -- ^ Response Message
  } deriving (Eq, Show)

data URI = URI
    { uriScheme     :: String        -- ^ Ex: http or https
    , uriAuthority  :: Maybe URIAuth -- ^ authority = [ userinfo "@" ] host [ ":" port ]
    , uriPath       :: String        -- ^ Path is the part between the
                                    -- authority and the query
    , uriQuery      :: String        -- ^ Query begins with '?'
    , uriFragment   :: String        -- ^ Fragment begins with '#'
    } deriving (Eq, Show)

data URIAuth = URIAuth
    { uriUserInfo   :: String  -- ^ username:password
    , uriRegName    :: String  -- ^ registered name, ex: www.core.gen.tr
    , uriPort       :: String  -- ^ Port as a string
    } deriving (Eq, Show)

nullURI :: URI
nullURI = URI
    { uriScheme     = ""
    , uriAuthority  = Nothing
    , uriPath       = ""
    , uriQuery      = ""
    , uriFragment   = ""
    }

-- | HTTP error.
data HttpError
    = InvalidRequestError { httpErrorMessage :: String }
      deriving (Eq, Typeable)

instance Exception HttpError

instance Show HttpError where
  show (InvalidRequestError msg) = "Invalid HTTP request: " ++ msg

