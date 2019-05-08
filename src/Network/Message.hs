{-# LANGUAGE OverloadedStrings #-}

{-  parsing emails according to Rfc 5322 and friends... -}

module Network.Message
  -- ( parseMessage )
  where

--------------------------------------------------------------------------------
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString as BS
import qualified Network.Parser.Rfc5322 as R5322
import qualified Network.Parser.Rfc2045 as R2045
import qualified Network.Parser.Rfc2046 as R2046
import Data.Map.Strict as M
import Data.Semigroup ((<>))
--------------------------------------------------------------------------------

-- * Aliases
type Message = R5322.Message
type ParsedMessage = (Message, [Message])

-- * Parsing
parseMessage
  :: BS.ByteString
  -> Either String ParsedMessage
parseMessage rawMessage
  = case parseOnly R5322.message rawMessage of
      Left  err -> Left ("Unable to parse raw email source: ++ " <> err)
      Right msg5322 -> case R5322.messageBody msg5322 of
        Nothing   -> Left "body not found in the message" 
        Just body -> case R5322.contentType msg5322 of
          Nothing -> Left "No content-type header in raw message!"
          Just ct -> if ("multipart/" `isPrefixOf` ct)
            then
            (case parseOnly ctParamsParser ct of
              Left  err         -> Left err -- TODO: handle no parameter case
              Right (mtype, ps) -> case hasBoundary ps of
                Nothing -> Left "content-type header doesn't declare a boundary" -- TODO: handle no boundary case
                Just b  -> case parseOnly (R2046.multipartBody b) body of
                  Left err -> Left $ "Unable to parse mime parts from the body: " ++ err ++ show b
                  Right ms -> Right (msg5322, ms))
            else
            (case R5322.contentLength msg5322 of
              Nothing -> Left "no content length detected!"
              Just l  -> Left $ "content-length found: " ++ show l) -- TODO: "text/plain; charset=us-ascii" by default
  where
    hasBoundary = M.lookup "boundary"
    ctParamsParser :: Parser (ByteString, (M.Map ByteString ByteString))
    ctParamsParser
      = do ty <- (\a b -> a <> "/" <> b) <$> R2045.mtype <* word8 47 <*> R2045.subtype
           ps <- AC.many' (R2045.semicolonsp *> _ctParam)
           return (ty, M.fromList ps)

_ctParam :: Parser (ByteString, ByteString)
_ctParam = res <$> (R2045.attribute <* word8 61)
              <*> (try (word8 34 *> R2045.value <* word8 34) <|> R2045.value)
  where res a v = (pack a, v)

-- >>> import Text.Pretty.Simple (pPrint, pPrintNoColor)
-- <interactive>:1:1: warning: [-Wdeprecations]
--     Module ‘Data.Attoparsec’ is deprecated:
--       This module will be removed in the next major release.
-- >>> msg <- Data.ByteString.Char8.readFile "tests/2046-1.txt"
-- >>> pPrintNoColor . parseMessage $ msg
-- Right 
--     ( Message 
--         { messageFields = 
--             [ From 
--                 [ NameAddress 
--                     { naName = Just "Nathaniel Borenstein" 
--                     , naAddr = "nsb@bellcore.com" 
--                     } 
--                 ]
--             , To 
--                 [ NameAddress 
--                     { naName = Just "Ned Freed" 
--                     , naAddr = "ned@innosoft.com" 
--                     } 
--                 ]
--             , Date 1993-03-22 07:56:48 UTC
--             , Subject "Sample message" 
--             , OptionalField "MIME-Version" "1.0" 
--             , OptionalField "Content-type" "multipart/mixed; boundary="simpleboundary"" 
--             ] 
--         , messageBody = Just "This is the preamble.  It is to be ignored, though it
--       is a handy place for composition agents to include an
--       explanatory note to non-MIME conformant readers.
--       
--       --simpleboundary
--       
--       This is implicitly typed plain US-ASCII text.
--       It does NOT end with a linebreak.
--       --simpleboundary
--       Content-type: text/plain; charset=us-ascii
--       
--       This is explicitly typed plain US-ASCII text.
--       It DOES end with a linebreak.
--       
--       --simpleboundary--
--       
--       This is the epilogue.  It is also to be ignored." 
--         } 
--     , 
--         [ Message 
--             { messageFields = []
--             , messageBody = Just "This is implicitly typed plain US-ASCII text.
--       It does NOT end with a linebreak." 
--             } 
--         , Message 
--             { messageFields = [ OptionalField "Content-type" "text/plain; charset=us-ascii" ]
--             , messageBody = Just "This is explicitly typed plain US-ASCII text.
--       It DOES end with a linebreak." 
--             } 
--         ] 
--     )
