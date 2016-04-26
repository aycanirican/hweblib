{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Parser.Mime
-- Copyright   :  Aycan iRiCAN 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- http://www.ietf.org/rfc/rfc2045.txt
-- http://www.ietf.org/rfc/rfc2046.txt

module Network.Parser.Mime where
--------------------------------------------------------------------------------
import           Control.Arrow              ((***))
import           Data.Attoparsec.ByteString
import           Data.ByteString
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Time
import           Prelude                    hiding (take, takeWhile)
--------------------------------------------------------------------------------
import           Network.Parser.Rfc2045
--------------------------------------------------------------------------------
-- We're converting mime types to Haskell Types in order to get rid of
-- string case conversion...

-- | * Utilities
string2mimetype :: ByteString -> MimeType
string2mimetype s =
    case paired s of
      ("text", s) -> Text s
      ("image", s) -> Image s
      ("audio", s) -> Audio s
      ("video", s) -> Video s
      ("application", s) -> Application s
      ("message", s) -> Message s
      ("multipart", s) ->
          case s of
            "alternative" -> MultiPart Alternative
            "byteranges" -> MultiPart Byteranges
            "digest" -> MultiPart Digest
            "encrypted" -> MultiPart Encrypted
            "formdata" -> MultiPart FormData
            "mixed" -> MultiPart Mixed
            "parallel" -> MultiPart Parallel
            "related" -> MultiPart Related
            "signed" -> MultiPart Signed
            _ -> MultiPart (Extension s)
      (t, s) -> Other t s
    where
      paired s = let (a,b) = (T.break (== '/') . T.toLower . decodeLatin1) s in
                 (a, T.drop 1 b)

-- Parse headers and map them to a MimeValue
parseMimeHeaders :: Parser MimeValue
parseMimeHeaders = do
  eh <- entityHeaders
  let mv = L.foldl f nullMimeValue eh
  return mv
  where
    bs2t = M.fromList . Prelude.map (decodeLatin1 *** decodeLatin1) . M.toList
    hVal = decodeLatin1 . hValue
    f z x =
        case hType x of
          IdH -> z { mvHeaders = M.insert IdH (hVal x) (mvHeaders z) }
          DescriptionH -> z { mvHeaders = M.insert DescriptionH (hVal x) (mvHeaders z) }
          VersionH -> z { mvHeaders = M.insert VersionH (hVal x) (mvHeaders z) }
          EncodingH -> z { mvHeaders = M.insert EncodingH (hVal x) (mvHeaders z) }
          ExtensionH e -> z { mvHeaders = M.insert (ExtensionH e) (hVal x) (mvHeaders z) }
          ContentH -> z { mvType = Type ((string2mimetype . hValue) x) (bs2t $ hParams x) }

-- | * Data Types
-----------------
-- Parts of the code:
-- Copyright : (c) 2006-2009, Galois, Inc.
-- License   : BSD3

-- | recursive at MimeContent, holding mime values
data MimeValue
    = MimeValue
      { mvType    :: Type
      , mvDisp    :: Maybe Disposition
      , mvContent :: MimeContent
      , mvHeaders :: M.Map HeaderType T.Text
      , mvIncType :: Bool
      } deriving (Eq, Show)

nullMimeValue :: MimeValue
nullMimeValue
    = MimeValue
      { mvType = nullType
      , mvDisp = Nothing
      , mvContent = Multi []
      , mvHeaders = M.empty
      , mvIncType = True
      }

data Type
    = Type
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

data Disposition
    = Disposition
      { dispType       :: DispType
      , dispParamTypes :: [DispParam]
      } deriving (Eq, Show)

data DispType
    = DispInline
    | DispAttachment
    | DispFormData
    | DispOther T.Text
      deriving (Eq, Show)

data DispParam
    = Name T.Text
    | Filename T.Text
    | CreationDate UTCTime
    | ModDate UTCTime
    | ReadDate UTCTime
    | Size Integer
    | OtherParam T.Text T.Text
      deriving (Eq, Show)
--------------------------------------------------------------------
