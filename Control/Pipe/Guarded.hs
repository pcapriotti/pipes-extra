{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Pipe.Guarded (
  Pipe,
  Producer,
  Consumer,
  Pipeline,
  yield,
  await,
  tryAwait,
  pipe,
  idP,
  Lazy(..),
  Strict(..),
  (<+<), (>+>),
  (<-<), (>->),
  discard,
  runPipe
  ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import qualified Control.Pipe as P
import Data.Maybe
import Prelude hiding ((.), id)

data PipeState = Running
               | Terminated
               | Guarded
  deriving Eq

newtype Pipe a b m r = Pipe { unPipe :: P.Pipe (Maybe a) (Maybe b) (StateT PipeState m) r }
  deriving (Monad, Applicative, Functor)

instance MonadTrans (Pipe a b) where
  lift = Pipe . lift . lift

type Consumer a m r = Pipe a P.Zero m r
type Producer b m r = Pipe P.Zero b m r
type Pipeline m r = Pipe P.Zero P.Zero m r

yield :: b -> Pipe a b m ()
yield = Pipe . P.yield . Just

await :: Monad m => Pipe a b m a
await = Pipe $ P.await >>= maybe (P.yield Nothing >> P.discard) return

tryAwait :: Monad m => Pipe a b m (Maybe a)
tryAwait = Pipe $ do
  s <- lift get
  when (s == Terminated) $ P.yield Nothing
  x <- P.await
  when (isNothing x && s == Running) $ lift (put Terminated)
  return x

pipe :: Monad m => (a -> b) -> Pipe a b m r
pipe f = forever $ await >>= yield . f

idP :: Monad m => Pipe a a m r
idP = pipe id

guarded :: Monad m => Pipe a b m r -> P.Pipe (Maybe a) (Maybe b) (StateT PipeState m) r
guarded p = do
  result <- unPipe p
  s <- lift get
  when (s /= Guarded) $ P.yield Nothing
  lift (put Guarded)
  return result

newtype Lazy m r a b = Lazy { unLazy :: Pipe a b m r }
instance Monad m => Category (Lazy m r) where
  id = Lazy idP
  (Lazy p1) . (Lazy p2) = Lazy . Pipe $ unPipe p1 P.<+< guarded p2

newtype Strict m r a b = Strict { unStrict :: Pipe a b m r }
instance Monad m => Category (Strict m r) where
  id = Strict idP
  (Strict p1) . (Strict p2) = Strict . Pipe $ unPipe p1 P.<-< guarded p2

(<+<) :: Monad m => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p1 <+< p2 = unLazy $ (Lazy p1) . (Lazy p2)

(>+>) :: Monad m => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
p2 >+> p1 = unLazy $ (Lazy p1) . (Lazy p2)

(<-<) :: Monad m => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p1 <-< p2 = unStrict $ (Strict p1) . (Strict p2)

(>->) :: Monad m => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
p2 >-> p1 = unStrict $ (Strict p1) . (Strict p2)

discard :: Monad m => Pipe a b m r
discard = forever await

runPipe :: Monad m => Pipeline m r -> m r
runPipe p = (`evalStateT` Running) . P.runPipe $ (P.pipe Just) P.>+> unPipe p P.>+> P.discard
