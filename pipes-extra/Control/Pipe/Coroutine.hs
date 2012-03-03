module Control.Pipe.Coroutine (
  Coroutine,
  coroutine,
  suspend,
  suspendE,
  resume,
  step,
  terminate
  ) where

import Control.Monad
import Control.Pipe
import qualified Control.Exception as E

data Coroutine a b m r = Coroutine
  { suspend :: Pipe a b m r
  , suspendE :: E.SomeException -> Pipe a b m r }

resume :: Monad m
       => Pipe a b m r
       -> Pipe a x m (Either r (b, Coroutine a b m r))
resume (Pure r) = return $ Left r
resume (Throw e) = throwP e
resume (Free c h) = go c >>= \x -> case x of
  Left p       -> resume p
  Right (b, p) -> return $ Right (b, Coroutine p h)
  where
    go (Await k) = liftM (Left . k) await
    go (Yield b p) = return $ Right (b, p)
    go (M m s) = liftM Left $ liftP s m

coroutine :: Monad m
          => Pipe a b m r
          -> Coroutine a b m r
coroutine p = Coroutine p throwP

step :: Monad m
     => Coroutine a b m r
     -> Pipe a x m (Either r (b, Coroutine a b m r))
step = resume . suspend

terminate :: Monad m
          => Coroutine a b m r
          -> Pipe a x m ()
terminate (Coroutine _ h) =
  void (catchP discard h) >+> return ()
