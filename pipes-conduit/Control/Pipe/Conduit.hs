-- | Adapters to convert conduits to pipes.
module Control.Pipe.Conduit (
  -- ** Sources
  sourcePipe,
  -- ** Conduits
  conduitPipe,
  conduitPipe_,
  -- ** Sinks
  sinkPipe,
  sinkPipe_
  ) where

import Control.Monad (void)
import Control.Monad.Trans.Resource
import Control.Pipe
import Data.Conduit

-- | Convert a 'Conduit' to 'Pipe'.
--
-- The resulting pipe behaves like the original 'Conduit', and closes it upon
-- termination. Any unconsumed input is returned.
conduitPipe :: Resource m => Conduit a m b -> Pipe a b (ResourceT m) (Maybe a)
conduitPipe (Conduit push close) = do
  x <- tryAwait
  case x of
    Nothing -> lift close >>= mapM_ yield >> return Nothing
    Just input -> do
      result <- lift $ push input
      case result of
        Producing c' output -> mapM_ yield output >> conduitPipe c'
        Finished input' output -> mapM_ yield output >> return input'

-- | Convert a 'Conduit' to a 'Pipe', ignoring unconsumed input.
conduitPipe_ :: Resource m => Conduit a m b -> Pipe a b (ResourceT m) ()
conduitPipe_ = void . conduitPipe

-- | Convert a 'Source' into a 'Pipe'.
--
-- The resulting 'Pipe' is a 'Producer' which pulls from the 'Source' until
-- exhaustion and yields the received data.
sourcePipe :: Resource m => Source m a -> Pipe x a (ResourceT m) ()
sourcePipe (Source pull _) = do
  result <- lift pull
  case result of
    Open s x -> yield x >> sourcePipe s
    Closed -> return ()

-- | Convert a 'Sink' into a 'Pipe'.
--
-- Optional consumed input is returned, together with the sink result.
sinkPipe :: Resource m => Sink a m b -> Pipe a x (ResourceT m) (Maybe a, b)
sinkPipe (SinkNoData out) = return (Nothing, out)
sinkPipe (SinkLift m) = lift m >>= sinkPipe
sinkPipe (SinkData p c) = go p c
  where
    go push close = do
      mx <- tryAwait
      case mx of
        Nothing -> do
          out <- lift close
          return (Nothing, out)
        Just x -> do
          result <- lift $ push x
          case result of
            Processing push' close' -> go push' close'
            Done input output -> return (input, output)

-- | Convert a 'Sink' into a 'Pipe', ignoring results.
sinkPipe_ :: Resource m => Sink a m b -> Pipe a x (ResourceT m) ()
sinkPipe_ = void . sinkPipe
