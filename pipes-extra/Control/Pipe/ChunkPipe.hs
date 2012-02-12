-- | This module contains utilities to create and combine pipes that accept
-- "chunked" input and return unconsumed portions of their internal buffer.
--
-- The main interface is an alternative monad instance for Pipe, which passes
-- leftover data along automatically.
module Control.Pipe.ChunkPipe (
  ChunkPipe(..),
  unChunkPipe,
  nonchunked,
  ) where

import Control.Pipe
import Data.Monoid

-- | Newtype wrapper for Pipe proving a monad instance that takes care of
-- passing leftover data automatically.
--
-- An individual 'ChunkPipe' is just a regular pipe, but returns unconsumed
-- input in a pair alongside the actual return value.
newtype ChunkPipe a b m r = ChunkPipe { unChunkPipe :: Pipe a b m (a, r) }

instance (Monoid a, Monad m) => Monad (ChunkPipe a b m) where
  return = nonchunked . return
  (ChunkPipe p) >>= f = ChunkPipe $ p >>= \(leftover, result) ->
    (yield leftover >> idP) >+> unChunkPipe (f result)

-- | Create a 'ChunkPipe' out of a regular pipe that is able to consume all its
-- input.
nonchunked :: (Monoid a, Monad m) => Pipe a b m r -> ChunkPipe a b m r
nonchunked p = ChunkPipe $ p >>= \r -> return (mempty, r)
