module Control.Pipe.PutbackPipe (
  nonputback,
  putback,
  yield,
  await,
  tryAwait,
  runPutback
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import qualified Control.Pipe as P
import Control.Pipe ((>+>), Pipe)
import qualified Control.Pipe.Combinators as PC
import Control.Pipe.Monoidal

newtype PutbackPipe a b m r = PutbackPipe {
  unPutback :: StateT [a] (Pipe a b m) r
  }

instance Monad m => Monad (PutbackPipe a b m) where
  return = PutbackPipe . return
  (PutbackPipe p) >>= f = PutbackPipe (p >>= unPutback . f)

instance MonadTrans (PutbackPipe a b) where
  lift = PutbackPipe . lift . lift

instance (Monad m, Functor m) => Functor (PutbackPipe a b m) where
  fmap f (PutbackPipe p) = PutbackPipe (fmap f p)

-- | FIXME: Move this to pipes-core
instance (MonadIO m) => MonadIO (Pipe a b m)  where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (PutbackPipe a b m) where
  liftIO a = PutbackPipe (liftIO a)

nonputback :: (Monad m) => Pipe a b m r -> PutbackPipe a b m r
nonputback p = PutbackPipe (lift p)

putback :: (Monad m) => a -> PutbackPipe a b m ()
putback a = PutbackPipe (modify (a:))

yield :: (Monad m) => b -> PutbackPipe a b m ()
yield = PutbackPipe . lift . P.yield

await  :: (Monad m) => PutbackPipe a b m a
await = PutbackPipe (get >>= decide) where
  decide [] = lift P.await
  decide (a:as) = put as >> return a

tryAwait  :: (Monad m) => PutbackPipe a b m (Maybe a)
tryAwait = PutbackPipe (get >>= decide) where
  decide [] = lift PC.tryAwait
  decide (a:as) = put as >> return (Just a)

runPutback :: (Monad m) => PutbackPipe a b m r -> Pipe a b m r
runPutback pb = evalStateT (unPutback pb) []

