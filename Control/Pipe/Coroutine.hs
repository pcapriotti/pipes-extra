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
import Control.Pipe.Exception
import qualified Control.Exception as E
import Data.Typeable
import Prelude hiding (catch)

data Coroutine a b m r = Coroutine
  { resume :: Pipe a b m r
  , finalizer :: [m ()]
  }

suspend :: Monad m
        => Pipe a b m r
        -> Pipe a x m (Either r (b, Coroutine a b m r))
suspend (Pure r w) = Pure (Left r) w
suspend (Throw e w) = Throw e w
suspend (Yield x p w) = return (Right (x, Coroutine p w))
suspend (M s m h) = M s (liftM suspend m) (suspend . h)
suspend (Await k h) = Await (suspend . k) (suspend . h)

coroutine :: Monad m
          => Pipe a b m r
          -> Coroutine a b m r
coroutine p = Coroutine p []

step :: Monad m
     => Coroutine a b m r
     -> Pipe a x m (Either r (b, Coroutine a b m r))
step = suspend . resume

terminate :: Monad m
          => Coroutine a b m r
          -> Pipe a b m ()
terminate p = mapM_ masked (finalizer p)
