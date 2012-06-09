{-# LANGUAGE DeriveDataTypeable #-}
module Control.Pipe.Coroutine (
  Coroutine,
  resume,
  suspend,
  coroutine,
  step,
  terminate
  ) where

import Control.Monad
import Control.Pipe
import Control.Pipe.Internal
import Data.Void
import Prelude hiding (catch)

data Coroutine a b m r = Coroutine
  { resume :: Pipe Void a b m r
  , finalizer :: [m ()]
  }

suspend :: Monad m
        => Pipe Void a b m r
        -> Pipe l a x m (Either r (b, Coroutine a b m r))
suspend (Pure r w) = Pure (Left r) w
suspend (Throw e p w) = Throw e (suspend p) w
suspend (Yield x p w) = return (Right (x, Coroutine p w))
suspend (M s m h) = M s (liftM suspend m) (suspend . h)
suspend (Await k h) = Await (suspend . k) (suspend . h)
suspend (Unawait x _) = absurd x
suspend (Flush p) = Flush (suspend p)

coroutine :: Monad m
          => Pipe Void a b m r
          -> Coroutine a b m r
coroutine p = Coroutine p []

step :: Monad m
     => Coroutine a b m r
     -> Pipe l a x m (Either r (b, Coroutine a b m r))
step = suspend . resume

terminate :: Monad m
          => Coroutine a b m r
          -> Pipe l a b m ()
terminate p = mapM_ masked (finalizer p)
