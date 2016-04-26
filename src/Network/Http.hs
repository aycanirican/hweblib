module Network.Http where

--------------------------------------------------------------------------------

import           Data.Attoparsec.ByteString
import           Data.ByteString.Char8      as C
import qualified Network.Parser.Rfc2046     as R2046
import           Network.Parser.Rfc7230
--import qualified Network.Parser.Rfc2822 as R2822
import           Network.Parser.Mime
import           Network.Types
--------------------------------------------------------------------------------

-- contentType :: Request -> Maybe ByteString
-- contentType = ehContentType . entityHeaders . rqHeaders

-- contentLength :: Request -> Maybe Int
-- contentLength = ehContentLength . entityHeaders . rqHreaders

parseMessage :: Parser HTTPMessage
parseMessage = http_message

-- TODO: case-insensitive lookup
hasHeader :: Request -> ByteString -> Maybe ByteString
hasHeader rq key = lookup key (rqHeaders rq)

mimes :: Request -> MimeContent
mimes rq = undefined

{-
Utku says:

* Maybe Type's should be exported per RFC instead of Network.Types
* Some simple types can have pretty printers (maybe just for debugging purposes, like "URI", "StatusLine" etc)
* querying uriQuery and uriFragment, cookie etc
-}
