{-# LANGUAGE DeriveDataTypeable #-}

module Network.Types 
       ( -- * HTTP Types
         Method(..)
       , HttpVersion
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

-- | HTTP error.
data HttpError
    = InvalidRequestError { httpErrorMessage :: String }
      deriving (Eq, Typeable)

instance Exception HttpError

instance Show HttpError where
  show (InvalidRequestError msg) = "Invalid HTTP request: " ++ msg

-- data HttpMessage = Request | Response
data Header = GeneralHeader | RequestHeader | EntityHeader
type HttpVersion = (Int,Int)
data Method = GET 
            | HEAD 
            | POST 
            | PUT 
            | DELETE 
            | TRACE 
            | OPTIONS 
            | CONNECT 
            | EXTENSIONMETHOD ByteString
              deriving (Show,Read,Ord,Eq)

data Request = 
  Request {
      rqMethod  :: Method                     -- ^ Request Method
    , rqUri     :: RequestUri                 -- ^ Request URI
    , rqVersion :: (Int, Int)                 -- ^ HTTP Version as a tuple
    , rqHeaders :: [(ByteString, ByteString)] -- ^ Request Headers as an alist
    , rqBody    :: ByteString                 -- ^ Request Body
    } deriving (Eq, Show)


data RequestUri = Asterisk 
                | AbsoluteUri URI
                | AbsolutePath ByteString
                | Authority (Maybe URIAuth)
                  deriving (Eq, Show)

data Response = 
  Response {
      rpCode    :: Int                        -- ^ Response Code
    , rpHeaders :: [(ByteString, ByteString)] -- ^ Response Headers as an alist
    , rpVersion :: (Int,Int)                  -- ^ HTTP Version
    , rpMessage :: ByteString                 -- ^ Response Message
  } deriving (Eq, Show)

data URI = URI
    { uriScheme     :: String
    , uriAuthority  :: Maybe URIAuth
    , uriPath       :: String
    , uriQuery      :: String
    , uriFragment   :: String
    } deriving (Eq, Typeable, Show)

data URIAuth = URIAuth
    { uriUserInfo   :: String
    , uriRegName    :: String
    , uriPort       :: String
    } deriving (Eq, Typeable, Show)

nullURI :: URI
nullURI = URI
    { uriScheme     = ""
    , uriAuthority  = Nothing
    , uriPath       = ""
    , uriQuery      = ""
    , uriFragment   = ""
    }
