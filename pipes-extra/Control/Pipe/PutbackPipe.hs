module Control.Pipe.PutbackPipe (
  nonputback,
  putback,
  yield,
  await,
  tryAwait,
  runPutback
  ) where

import Control.Monad.Trans
import qualified Control.Pipe as P
import Control.Pipe ((>+>), Pipe)
import qualified Control.Pipe.Combinators as PC
import Control.Pipe.Monoidal

newtype PutbackPipe a b m r = PutbackPipe {
  unPutback :: Pipe a (Either b a) m r
  }

instance Monad m => Monad (PutbackPipe a b m) where
    return = PutbackPipe . return
    (PutbackPipe p) >>= f = PutbackPipe (p >>= unPutback . f)

instance MonadTrans (PutbackPipe a b) where
  lift = PutbackPipe . lift

nonputback :: Monad m => Pipe a b m r -> PutbackPipe a b m r
nonputback p = PutbackPipe (p >+> P.pipe Left)

putback :: Monad m => a -> PutbackPipe a b m ()
putback = PutbackPipe . P.yield . Right

yield :: Monad m => b -> PutbackPipe a b m ()
yield = PutbackPipe . P.yield . Left

await :: Monad m => PutbackPipe a b m a
await = PutbackPipe P.await

tryAwait :: Monad m => PutbackPipe a b m (Maybe a)
tryAwait = PutbackPipe PC.tryAwait

runPutback :: Monad m => PutbackPipe a b m r -> Pipe a b m r
runPutback pb = loopP (joinP >+> unPutback pb)
