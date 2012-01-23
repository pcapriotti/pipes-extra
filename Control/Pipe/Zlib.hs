module Control.Pipe.Zlib (
  gzip,
  ungzip,
  decompress,
  compress
  ) where

-- adapted from conduit

import Codec.Zlib
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Pipe.Guarded
import Data.ByteString as B

-- | Gzip compression with default parameters.
gzip :: MonadIO m => Pipe B.ByteString B.ByteString m ()
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
ungzip :: MonadIO m => Pipe B.ByteString B.ByteString m ()
ungzip = decompress (WindowBits 31)

decompress
    :: MonadIO m
    => WindowBits
    -> Pipe B.ByteString B.ByteString m ()
decompress config = do
    inf <- lift . liftIO $ initInflate config
    whileAwait $ \x -> do
      chunks <- lift . liftIO $ withInflateInput inf x callback
      mapM_ yield chunks

    chunk <- lift . liftIO $ finishInflate inf
    unless (B.null chunk) $ do
      yield chunk

compress
    :: MonadIO m
    => Int
    -> WindowBits
    -> Pipe B.ByteString B.ByteString m ()
compress level config = do
    def <- lift . liftIO $ initDeflate level config
    whileAwait $ \x -> do
      chunks <- lift . liftIO $ withDeflateInput def x callback
      mapM_ yield chunks
    chunks <- lift . liftIO $ finishDeflate def callback
    mapM_ yield chunks

callback :: Monad m => m (Maybe a) -> m [a]
callback pop = go id where
    go xs = do
       x <- pop
       case x of
           Nothing -> return $ xs []
           Just y -> go (xs . (y:))

whileAwait :: Monad m => (a -> Pipe a b m r) -> Pipe a b m ()
whileAwait f = do
  x <- tryAwait
  case x of
    Nothing -> return ()
    Just x' -> f x' >> whileAwait f
