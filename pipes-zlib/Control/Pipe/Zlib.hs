{-# LANGUAGE ScopedTypeVariables #-}

module Control.Pipe.Zlib (
  gzip,
  gunzip,
  decompress,
  compress
  ) where

-- adapted from conduit

import Codec.Zlib
import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Pipe
import qualified Data.ByteString as B
import Prelude hiding (catch)

-- | Gzip compression with default parameters.
gzip :: MonadIO m => Pipe B.ByteString B.ByteString m ()
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
gunzip :: MonadIO m => Pipe B.ByteString B.ByteString m ()
gunzip = decompress (WindowBits 31)

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
    unless (B.null chunk) $
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

callback :: (Show a, MonadIO m) => m (Maybe a) -> m [a]
callback pop = go id where
  go xs = do
    x <- pop
    case x of
      Nothing -> return $ xs []
      Just y -> go (xs . (y:))

whileAwait :: (Show a, MonadIO m) => (a -> Pipe a b m r) -> Pipe a b m ()
whileAwait f = catch (forever $ await >>= f)
  (\(_ :: BrokenUpstreamPipe) -> return ())
