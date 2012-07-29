{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Control.Pipe.Binary (
  -- ** Handle and File IO
  fileReader,
  handleReader,
  fileWriter,
  handleWriter,

  -- ** Chunked Byte Stream Manipulation
  lines,
  bytes,
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Pipe
import Control.Pipe.Class
import Control.Pipe.Exception
import Control.Pipe.Combinators (feed)
import qualified Data.ByteString as B
import Data.Word
import System.IO
import Prelude hiding (take, takeWhile, dropWhile, lines, catch)

import qualified Control.Pipe.Chunked as C

-- | Read data from a file.
fileReader :: (MonadStream m, MonadIO (BaseMonad m))
           => FilePath -> m () B.ByteString u ()
fileReader path = liftPipe $ bracket
  (liftIO $ openFile path ReadMode)
  (liftIO . hClose)
  handleReader

-- | Read data from an open handle.
handleReader :: (MonadStream m, MonadIO (BaseMonad m))
             => Handle -> m () B.ByteString u ()
handleReader h = go
  where
    go = do
      eof <- liftIO $ hIsEOF h
      unless eof $ do
        chunk <- liftIO $ B.hGetSome h 4096
        yield chunk
        go

-- | Write data to a file.
--
-- The file is only opened if some data arrives into the pipe.
fileWriter :: (MonadStream m, MonadIO (BaseMonad m))
           => FilePath -> m B.ByteString Void r r
fileWriter path = withDefer $ do
  -- receive some data before opening the handle
  input <- await
  -- feed it to the actual worker pipe
  feed input go
  where
    go = bracket
      (liftIO $ openFile path WriteMode)
      (liftIO . hClose)
      handleWriter

-- | Write data to a handle.
handleWriter :: (MonadStream m, MonadIO (BaseMonad m))
             => Handle -> m B.ByteString Void r r
handleWriter h = forP $ liftIO . B.hPut h


-- | Split the chunked input stream into lines, and yield them individually.
lines :: MonadStream m => m B.ByteString B.ByteString r r
lines = C.splitOn (10 :: Word8)

-- | Yield individual bytes of the chunked input stream.
bytes :: MonadStream m => m B.ByteString Word8 r r
bytes = C.elems
