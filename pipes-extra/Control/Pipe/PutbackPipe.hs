-- | This module contains an alternative pipe implementation, 'PutbackPipe',
-- providing an additional primitive 'putback', which allows data to be
-- inserted into the input stream of the current pipe.
--
-- PutbackPipes can be used to implement pipes with left-over data, and can be
-- composed vertically (using the Monad instance), but not horizontally.
--
-- To make use of a PutbackPipe within a 'Pipeline', you need to convert it to
-- a regular 'Pipe' using 'runPutback'.
module Control.Pipe.PutbackPipe (
  fromPipe,
  putback,
  yield,
  await,
  tryAwait,
  runPutback
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Control.Pipe as P
import Control.Pipe ((>+>), Pipe)
import qualified Control.Pipe.Combinators as PC
import Control.Pipe.Monoidal

-- | The 'PutbackPipe' data type.
newtype PutbackPipe a b m r = PutbackPipe {
  unPutback :: Pipe (Either a a) (Either b a) m r
  }

instance Monad m => Monad (PutbackPipe a b m) where
    return = PutbackPipe . return
    (PutbackPipe p) >>= f = PutbackPipe (p >>= unPutback . f)

instance MonadTrans (PutbackPipe a b) where
  lift = PutbackPipe . lift

instance Monad m => Functor (PutbackPipe a b m) where
  fmap f (PutbackPipe p) = PutbackPipe (liftM f p)

instance Monad m => Applicative (PutbackPipe a b m) where
  pure = return
  (<*>) = ap

instance MonadIO m => MonadIO (PutbackPipe a b m) where
  liftIO a = PutbackPipe (liftIO a)

-- | Create a 'PutbackPipe' from a regular pipe.
fromPipe :: Monad m => Pipe a b m r -> PutbackPipe a b m r
fromPipe p = PutbackPipe (joinP >+> p >+> P.pipe Left)

-- | Put back an element into the input stream.
putback :: Monad m => a -> PutbackPipe a b m ()
putback = PutbackPipe . P.yield . Right

-- | Same as 'P.yield' for regular pipes.
yield :: Monad m => b -> PutbackPipe a b m ()
yield = PutbackPipe . P.yield . Left

-- | Same as 'P.await' for regular pipes.
await :: Monad m => PutbackPipe a b m a
await = PutbackPipe $ liftM (either id id) P.await

-- | Same as 'PC.tryAwait' for regular pipes.
tryAwait :: Monad m => PutbackPipe a b m (Maybe a)
tryAwait = PutbackPipe $ liftM (fmap (either id id)) PC.tryAwait

-- | Convert a 'PutbackPipe' to a regular pipe.
runPutback :: Monad m => PutbackPipe a b m r -> Pipe a b m r
runPutback = loopP . unPutback
