{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.Monoid.MonoidSplit where

import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Word

import qualified Prelude
import Prelude hiding (take, drop, length)

import Data.Generator.Length

-- | A parameterized set of splits of a monoid.
--
-- A split is a section f = t *** d of the monoid operation such that:
--
-- > forall m m'. d m /= mempty -> f (m <> m') = (t m, d m <> m')
--
-- The set of splits of a monoid has a natural structure of bounded poset.
--
-- Given a poset @s@, a 'MonoidSplit s m' instance is an homomorphism from @s@
-- to the poset of splits of @m@.
-- 
-- Explicitly, 
--
-- > forall x m. uncurry mappend (split x m) == m
-- > forall x m m'. drop x m /= mempty
-- >              -> split x (m <> m') == (take x m, drop x m <> m')
-- > forall x y m. x <= y -> take x (take y m) == take x m
--
class Monoid m => MonoidSplit s m where
  split :: s -> m -> (m, m)
  split s m = (take s m, drop s m)

  take :: s -> m -> m
  take s = fst . split s

  drop :: s -> m -> m
  drop s = snd . split s

-- splits by length

instance MonoidSplit Int [a] where
  split = splitAt
  take = Prelude.take
  drop = Prelude.drop

instance MonoidSplit Int B.ByteString where
  split = B.splitAt
  take = B.take
  drop = B.drop

instance MonoidSplit Int T.Text where
  split = T.splitAt
  take = T.take
  drop = T.drop

-- splits by predicate

instance MonoidSplit (a -> Bool) [a] where
  split = span
  take = takeWhile
  drop = dropWhile

instance MonoidSplit (Word8 -> Bool) B.ByteString where
  split = B.span
  take = B.takeWhile
  drop = B.dropWhile

instance MonoidSplit (Char -> Bool) T.Text where
  split = T.span
  take = T.takeWhile
  drop = T.dropWhile

-- break on: split c == split (/= c)

instance MonoidSplit Word8 B.ByteString where
  split = B.breakByte

instance MonoidSplit Char T.Text where
  split c = split (/= c)

-- reverse splits

dualLength :: GeneratorLength g => g -> Int -> Int
dualLength s n = length s - n

instance (GeneratorLength m, MonoidSplit Int m) => MonoidSplit Int (Dual m) where
  split n (Dual s) = case split (dualLength s n) s of
    (s1, s2) -> (Dual s2, Dual s1)
  take n (Dual s) = Dual $ drop (dualLength s n) s
  drop n (Dual s) = Dual $ take (dualLength s n) s

instance MonoidSplit (Word8 -> Bool) (Dual B.ByteString) where
  split p (Dual s) = case B.spanEnd p s of
    (s1, s2) -> (Dual s2, Dual s1)

instance MonoidSplit (Char -> Bool) (Dual T.Text) where
  drop p (Dual s) = Dual $ T.dropWhileEnd p s
  take p (Dual s) = Dual $ drop (T.length (T.dropWhileEnd p s)) s
