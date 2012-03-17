{-# LANGUAGE DeriveDataTypeable #-}
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
import Control.Pipe.Exception
import qualified Control.Exception as E
import Data.Typeable
import Prelude hiding (catch)

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

data CoroutineTerminated = CoroutineTerminated
  deriving (Show, Typeable)

instance E.Exception CoroutineTerminated

terminate :: Monad m
          => Coroutine a b m r
          -> Pipe a b m ()
terminate p = go (suspendE p (E.toException CoroutineTerminated))
  where
    go (Pure r) = return ()
    go (Throw e) = return ()
    go (Free c h) = catchP (step c) (return . h) >>= go

    step (Await k) = liftM k await
    step (Yield b p) = return p
    step (M m (Finalizer _)) = masked m
    step (M m s) = liftP s m
