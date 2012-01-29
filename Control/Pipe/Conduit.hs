module Control.Pipe.Conduit (
  sourcePipe,
  conduitPipe,
  conduitPipe_,
  sinkPipe,
  sinkPipe_
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Pipe.Guarded
import Data.Conduit

conduitPipe :: Resource m => Conduit a m b -> Pipe a b (ResourceT m) (Maybe a)
conduitPipe c = do
  (PreparedConduit push close) <- lift $ prepareConduit c
  go push close
  where
    go push close = do
      x <- tryAwait
      case x of
        Nothing -> lift close >>= mapM_ yield >> return Nothing
        Just input -> do
          result <- lift $ push input
          case result of
            Producing output -> mapM_ yield output >> go push close
            Finished input' output -> mapM_ yield output >> return input'

conduitPipe_ :: Resource m => Conduit a m b -> Pipe a b (ResourceT m) ()
conduitPipe_ c = conduitPipe c >> return ()

sourcePipe :: Resource m => Source m a -> Pipe x a (ResourceT m) ()
sourcePipe s = do
  (PreparedSource pull close) <- lift $ prepareSource s
  go pull
  where
    go pull = do
      result <- lift pull
      case result of
        Open x -> yield x >> go pull
        Closed -> return ()

sinkPipe :: Resource m => Sink a m b -> Pipe a x (ResourceT m) (Maybe a, b)
sinkPipe s = do
  ps <- lift $ prepareSink s
  case ps of
    SinkNoData out -> return (Nothing, out)
    SinkData push close -> go push close
  where
    go push close = do
      x <- tryAwait
      case x of
        Nothing -> do
          out <- lift close
          return (Nothing, out)
        Just x -> do
          result <- lift $ push x
          case result of
            Processing -> go push close
            Done input output -> return (input, output)

sinkPipe_ :: Resource m => Sink a m b -> Pipe a x (ResourceT m) ()
sinkPipe_ s = sinkPipe s >> return ()
