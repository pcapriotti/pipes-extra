{-# LANGUAGE DeriveDataTypeable #-}
module Control.Pipe.Coroutine (
  Coroutine,
  resume,
  suspend,
  coroutine,
  step,
  terminate,
  suspendYield,
  stepYield,
  suspendAwait,
  stepAwait,
  suspendBoth,
  stepBoth
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

suspendBoth :: (Monad m)
    => (Maybe a)
    -> Pipe a b m r
    -> Pipe x y m (Maybe a, Either r (Maybe b,Coroutine a b m r))
suspendBoth v (Pure r w) = Pure (v, Left r) w
suspendBoth _ (Throw e w) = Throw e w
suspendBoth v (Yield x p w) = return (v, Right (Just x, Coroutine p w))
suspendBoth v (M s m h) = M s (liftM (suspendBoth v) m) (suspendBoth v . h)
suspendBoth (Just a) (Await k h) = suspendBoth Nothing (catchP (k a) h)
suspendBoth Nothing p@(Await k h) = return (Nothing, Right (Nothing,coroutine p))

stepBoth :: (Monad m)
    => Maybe a
    -> Coroutine a b m r
    -> Pipe x y m (Maybe a, Either r (Maybe b,Coroutine a b m r))
stepBoth ma = suspendBoth ma . resume

suspendYield :: Monad m
        => Pipe a b m r
        -> Pipe a x m (Either r (b, Coroutine a b m r))
suspendYield = suspend

stepYield :: Monad m
     => Coroutine a b m r
     -> Pipe a x m (Either r (b, Coroutine a b m r))
stepYield = step

suspendAwait :: (Monad m)
    => Maybe a
    -> Pipe a b m r
    -> Pipe x b m (Maybe a, Either r (Coroutine a b m r))
suspendAwait a p = suspendBoth a p >>= unSuspend where
    unSuspend (a, Left r) = return (a, Left r)
    unSuspend (a, Right (Just b, cr)) = yield b >> suspendAwait a (resume cr) 
    unSuspend (a, Right (Nothing, cr)) = return (a, Right cr)

stepAwait a = suspendAwait a . resume 

