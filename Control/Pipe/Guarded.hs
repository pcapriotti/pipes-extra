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
import Control.Monad.Trans
import qualified Control.Pipe as P
import Prelude hiding ((.), id)

newtype Pipe a b m r = Pipe { unPipe :: P.Pipe (Maybe a) b m r }
  deriving (Monad, Applicative, Functor, MonadTrans)

type Consumer a m r = Pipe a P.Zero m r
type Producer b m r = Pipe P.Zero b m r
type Pipeline m r = Pipe P.Zero P.Zero m r

yield :: b -> Pipe a b m ()
yield = Pipe . P.yield

await :: Monad m => Pipe a b m a
await = Pipe $ do
  x <- P.await
  case x of
    -- if the upstream pipe terminated, await again
    -- so that we also terminate with the same result
    Nothing -> P.await >> error "Pipe: internal error"
    Just x' -> return x'

tryAwait :: Pipe a b m (Maybe a)
tryAwait = Pipe P.await

pipe :: Monad m => (a -> b) -> Pipe a b m r
pipe f = forever $ await >>= yield . f

idP :: Monad m => Pipe a a m r
idP = pipe id

guarded :: Monad m => P.Pipe a b m r -> P.Pipe a (Maybe b) m r
guarded p = do
  result <- p P.>+> P.pipe Just
  P.yield Nothing
  return result

newtype Lazy m r a b = Lazy { unLazy :: Pipe a b m r }
instance Monad m => Category (Lazy m r) where
  id = Lazy idP
  (Lazy (Pipe p1)) . (Lazy (Pipe p2)) = Lazy . Pipe $ p1 P.<+< guarded p2

newtype Strict m r a b = Strict { unStrict :: Pipe a b m r }
instance Monad m => Category (Strict m r) where
  id = Strict idP
  (Strict (Pipe p1)) . (Strict (Pipe p2)) = Strict . Pipe $ p1 P.<-< guarded p2

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
runPipe (Pipe p) = P.runPipe $ (P.pipe Just) P.>+> p
