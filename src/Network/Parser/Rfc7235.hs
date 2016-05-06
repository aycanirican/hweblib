{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Module      :  Network.Parser.7235
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Hypertext Transfer Protocol (HTTP/1.1): Authentication
--
-- <http://www.ietf.org/rfc/rfc7235.txt>

module Network.Parser.Rfc7235 where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Attoparsec.ByteString       as BS
import           Data.Attoparsec.ByteString.Char8 as AC
import           Data.Attoparsec.Combinator
import           Data.ByteString
import           Data.Char                        (digitToInt)
import           Data.Monoid
import           Data.Scientific
import           Data.Time
import           Data.Typeable
import qualified GHC.Generics                     as GHC
import           Prelude                          hiding (product)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODO: implement it....
