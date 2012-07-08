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
import Prelude hiding (catch)

data Coroutine m a b u r = Coroutine
  { resume :: Pipe m a b u r
  , finalizer :: [m ()]
  }

suspend :: Monad m
        => Pipe m a b u r
        -> Pipe m a x u (Either r (b, Coroutine m a b u r))
suspend (Pure r w) = Pure (Left r) w
suspend (Throw e w) = Throw e w
suspend (Yield x p w) = return (Right (x, Coroutine p w))
suspend (M s m h) = M s (liftM suspend m) (suspend . h)
suspend (Await k j h w) = Await (suspend . k) (suspend . j) (suspend . h) w

coroutine :: Monad m
          => Pipe m a b u r
          -> Coroutine m a b u r
coroutine p = Coroutine p []

step :: Monad m
     => Coroutine m a b u r
     -> Pipe m a x u (Either r (b, Coroutine m a b u r))
step = suspend . resume

terminate :: Monad m
          => Coroutine m a b u r
          -> Pipe m a b u ()
terminate p = mapM_ masked (finalizer p)
