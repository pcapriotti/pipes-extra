-- | Guarded pipes are a drop-in replacement for pipes providing an additional
-- primitive 'tryAwait', which returns 'Nothing' when the upstream pipe
-- terminates.
--
-- This can be used by stateful pipes to yield additional data or compute a
-- return value just before termination.
--
-- Note that there is no guarantee that 'tryAwait' will actually return
-- 'Nothing' at some point. If downstream terminates first, the pipe will just
-- terminate immediately.
--
-- That means that 'tryAwait' cannot be reliably used as a mechanism to enforce
-- finalization of scarse resources, although it works well to make sure that
-- resources are finalized as soon as possible.
--
-- For example:
--
-- > foldP :: Monad m => (a -> b -> a) -> a -> Consumer b m a
-- > foldP f z = go z
-- >   where go z = tryAwait >>= maybe (return z) (go . f z)
--
-- uses 'tryAwait' to return the accumulator just before termination. There is
-- no way to implement 'foldP' with regular pipes, since 'await' causes the
-- pipe to terminate as soon as upstream terminates, without giving it a chance
-- to return a result.
module Control.Pipe.Guarded (
  module Control.Pipe.Common
  ) where

import Control.Pipe.Common
