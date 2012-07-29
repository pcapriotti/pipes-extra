{-# LANGUAGE FlexibleContexts #-}

module Control.Pipe.Chunked
  ( gtake
  , take
  , takeWhile
  , dropWhile
  , splitOn
  , elems
  ) where

import Control.Monad
import Control.Pipe
import Control.Pipe.Class
import Control.Pipe.Combinators (tryAwait)

import Data.Generator
import Data.Generator.Length
import Data.Monoid
import Data.Monoid.MonoidEq
import Data.Monoid.MonoidSplit hiding (take)

import Prelude hiding (null, take, drop, takeWhile, dropWhile, length)

gtake :: (MonadStream m, MonoidEq a, MonoidSplit s a)
      => (s -> Bool) -> (a -> s -> s) -> s -> m a a r a
gtake done update s
  | done s = return mempty
  | otherwise = tryAwait >>= maybe (return mempty) split_chunk
  where
    split_chunk chunk = do
      let (chunk', leftover) = split s chunk
      yield chunk'
      if null leftover
        then gtake done update (update chunk' s)
        else return leftover

-- | Act as an identity for the first 'n' elements of the chunk, then terminate
-- returning the unconsumed portion of the last chunk.
take :: (MonadStream m, MonoidEq a, GeneratorLength a, MonoidSplit Int a)
     => Int -> m a a u a
take = gtake (<= 0) (\chunk n -> n - length chunk)

-- | Act as an identity as long as the given predicate holds, then terminate
-- returning the unconsumed portion of the last chunk.
takeWhile :: (MonadStream m, MonoidEq a, MonoidSplit (c -> Bool) a)
          => (c -> Bool) -> m a a u a
takeWhile = gtake (const False) (const id)

-- | Drop elemetns as long as the given predicate holds, then act as an identity
dropWhile :: (MonadStream m, MonoidEq a, MonoidSplit (c -> Bool) a)
          => (c -> Bool) -> m a a r r
dropWhile p = do
  leftover <- takeWhile (not . p) >+> discard
  yield leftover
  idP

-- | Split stream on the given element
splitOn :: (MonadStream m, MonoidEq a, MonoidSplit c a, MonoidSplit Int a)
        => c -> m a a r r
splitOn c = go mempty
  where
    go leftover = do
      echunk <- awaitE
      case echunk of
        Left r -> do
          unless (null leftover) (yield leftover)
          return r
        Right chunk -> split_chunk chunk leftover
    split_chunk chunk leftover
      | null chunk = go leftover
      | null rest  = go (leftover <> chunk)
      | otherwise  = do yield (leftover <> line)
                        split_chunk (drop (1::Int) rest) mempty
        where (line, rest) = split c chunk

-- | Yield individual elements of the stream
elems :: (MonadStream m, Generator a) => m a (Elem a) r r
elems = forP $ \chunk -> mapM_ yield (reduce chunk)
