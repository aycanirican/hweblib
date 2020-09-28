{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      :  Network.Parser.7231
-- Copyright   :  Aycan iRiCAN, Utku Demir 2010-2020
-- License     :  BSD3
--
-- Maintainer  :  iricanaycan@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Permutation Parsers described in:
--
-- /Parsing Permutation Phrases,/
-- by Arthur Baars, Andres Loh and Doaitse Swierstra.
--
-- Collage from permute package and parsers package.
--
-- <http://www.ietf.org/rfc/rfc7231.txt>

module Network.Parser.Permute
  ( PermParser -- abstract
  , permute
  , (<||>), (<$$>)
  , (<|?>), (<$?>)
  ) where

import Control.Applicative
import Data.Foldable (asum)

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>

(<||>) :: Functor m => PermParser m (a -> b) -> m a -> PermParser m b
(<||>) = add

(<$$>) :: Functor m => (a -> b) -> m a -> PermParser m b
(<$$>) f p = newperm f <||> p

(<|?>) :: Functor m => PermParser m (a -> b) -> (a, m a) -> PermParser m b
(<|?>) perm (x,p) = addopt perm x p

(<$?>) :: Functor m => (a -> b) -> (a, m a) -> PermParser m b
(<$?>) f (x,p) = newperm f <|?> (x,p)

--------------------------------------------------------------------------------
data PermParser m a = Perm (Maybe a) [StreamBranch m a]

instance Functor m => Functor (PermParser m) where
  fmap f (Perm x xs) = Perm (fmap f x) (fmap f <$> xs)

data StreamBranch m a = forall b. Branch (PermParser m (b -> a)) (m b)

instance Functor m => Functor (StreamBranch m) where
  fmap f (Branch perm p) = Branch (fmap (f.) perm) p
--------------------------------------------------------------------------------
permute :: (Alternative m) => PermParser m a -> m a
permute (Perm def xs)
  = asum (map branch xs ++ e)
  where
    e = maybe [] (pure . pure) def
    branch (Branch perm p) = flip id <$> p <*> permute perm

newperm :: (a -> b) -> PermParser m (a -> b)
newperm f = Perm (Just f) []

add :: Functor m => PermParser m (a -> b) -> m a -> PermParser m b
add perm@(Perm _mf fs) p = Perm Nothing (first:map insert fs)
  where first                    = Branch perm p
        insert (Branch perm' p') = Branch (add (fmap flip perm') p) p'

addopt :: Functor m => PermParser m (a -> b) -> a -> m a -> PermParser m b
addopt perm@(Perm mf fs) x p = Perm (fmap ($ x) mf) (first:map insert fs)
  where first                    = Branch perm p
        insert (Branch perm' p') = Branch (addopt (fmap flip perm') x p) p'
--------------------------------------------------------------------------------
