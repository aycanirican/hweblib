{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.Mime
-- Copyright   :  Aycan iRiCAN 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- http://www.ietf.org/rfc/rfc2045.txt
-- http://www.ietf.org/rfc/rfc2046.txt
module Network.Parser.Mime 
  where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, many', try, word8)
import Data.ByteString (ByteString, pack)
import qualified Data.Map as M
import qualified Data.Text as T
import Network.Parser.Rfc2045
  ( HeaderType,
    attribute,
    mtype,
    semicolonsp,
    subtype,
    value,
  )
import Network.Parser.Rfc2183 (Disposition)
import Prelude hiding (take, takeWhile)

-- We're converting mime types to Haskell Types in order to get rid of
-- string case conversion...

-- | * Utilities
-- string2mimetype :: ByteString -> MimeType
-- string2mimetype s =
--     case paired s of
--       ("text", x) -> Text x
--       ("image", x) -> Image x
--       ("audio", x) -> Audio x
--       ("video", x) -> Video x
--       ("application", x) -> Application x
--       ("message", x) -> Message x
--       ("multipart", x) ->
--           case s of
--             "alternative" -> MultiPart Alternative
--             "byteranges" -> MultiPart Byteranges
--             "digest" -> MultiPart Digest
--             "encrypted" -> MultiPart Encrypted
--             "formdata" -> MultiPart FormData
--             "mixed" -> MultiPart Mixed
--             "parallel" -> MultiPart Parallel
--             "related" -> MultiPart Related
--             "signed" -> MultiPart Signed
--             _ -> MultiPart (Extension x)
--       (t, x) -> Other t x
--     where
--       paired x = let (a,b) = (T.break (== '/') . T.toLower . decodeLatin1) x in
--                  (a, T.drop 1 b)

-- Parse headers and map them to a MimeValue
-- parseMimeHeaders :: Parser MimeValue
-- parseMimeHeaders = do
--   eh <- entityHeaders
--   let mv = L.foldl f nullMimeValue eh
--   return mv
--   where
--     bs2t = M.fromList . Prelude.map (decodeLatin1 *** decodeLatin1) . M.toList
--     hVal = decodeLatin1 . hValue
--     f z x =
--         case hType x of
--           IdH          -> z { mvHeaders = M.insert IdH (hVal x) (mvHeaders z) }
--           DescriptionH -> z { mvHeaders = M.insert DescriptionH (hVal x) (mvHeaders z) }
--           VersionH     -> z { mvHeaders = M.insert VersionH (hVal x) (mvHeaders z) }
--           EncodingH    -> z { mvHeaders = M.insert EncodingH (hVal x) (mvHeaders z) }
--           ExtensionH e -> z { mvHeaders = M.insert (ExtensionH e) (hVal x) (mvHeaders z) }
--           ContentH     -> z { mvType    = Type ((string2mimetype . hValue) x) (bs2t $ hParams x) }

contentType :: Parser (ByteString, M.Map ByteString ByteString)
contentType = do
    ty <- (\a b -> a <> "/" <> b) <$> mtype <* word8 47 <*> subtype
    ps <- many' (semicolonsp *> singleParam)
    return (ty, M.fromList ps)

singleParam :: Parser (ByteString, ByteString)
singleParam =
  res <$> (attribute <* word8 61)
      <*> (try (word8 34 *> value <* word8 34) <|> value)
  where
    res a v = (pack a, v)

-- contentDisposition :: Parser Disposition
-- contentDisposition
--   = Disposition <$> dispositionType <*> many (";" *> lwsp *> dispositionParam)

-- | * Data Types

-----------------
-- Parts of the code:
-- Copyright : (c) 2006-2009, Galois, Inc.
-- License   : BSD3

-- | recursive at MimeContent, holding mime values
data MimeValue = MimeValue
  { mvType    :: Type
  , mvDisp    :: Maybe Disposition
  , mvContent :: MimeContent
  , mvHeaders :: M.Map HeaderType T.Text
  , mvIncType :: Bool
  }
  deriving (Eq, Show)

nullMimeValue :: MimeValue
nullMimeValue = MimeValue
    { mvType    = nullType
    , mvDisp    = Nothing
    , mvContent = Multi []
    , mvHeaders = M.empty
    , mvIncType = True
    }

data Type = Type
  { mimeType   :: MimeType
  , mimeParams :: M.Map T.Text T.Text
  } deriving (Eq, Show)

-- | content-type default as defined in rfc 2045
--   "Content-type: text/plain; charset=us-ascii"
nullType :: Type
nullType = Type (Text "plain") (M.fromList [("charset", "us-ascii")])

type SubType = T.Text
type TextType = T.Text

data MimeType
  = Text TextType
  | Image SubType
  | Audio SubType
  | Video SubType
  | Application SubType
  | Message SubType
  | MultiPart Multipart
  | Other T.Text SubType
  deriving (Eq, Show)

data Multipart
  = Alternative
  | Byteranges
  | Digest
  | Encrypted
  | FormData
  | Mixed
  | Parallel
  | Related
  | Signed
  | Extension T.Text
  | OtherMultiPart T.Text
  deriving (Eq, Show)

type Content = ByteString

data MimeContent
  = Single Content
  | Multi [MimeValue]
  deriving (Eq, Show)

--------------------------------------------------------------------
