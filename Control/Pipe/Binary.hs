{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Control.Pipe.Binary (
  -- ** Handle and File IO
  fileReader,
  handleReader,
  fileWriter,
  handleWriter,

  -- ** Chunked Byte Stream Manipulation
  take,
  takeWhile,
  dropWhile,
  lines,
  bytes,
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Pipe
import Control.Pipe.Class
import Control.Pipe.Exception
import Control.Pipe.Combinators (tryAwait, feed)
import qualified Data.ByteString as B
import Data.Monoid
import Data.Word
import System.IO
import Prelude hiding (take, takeWhile, dropWhile, lines, catch)

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

-- | Act as an identity for the first 'n' bytes, then terminate returning the
-- unconsumed portion of the last chunk.
take :: MonadStream m => Int -> m B.ByteString B.ByteString u B.ByteString
take 0 = return B.empty
take size = go
  where
    go = tryAwait >>= maybe (return B.empty) split
    split chunk = do
      let (chunk', leftover) = B.splitAt size chunk
      yield chunk'
      if B.null leftover
        then take $ size - B.length chunk'
        else return leftover

-- | Act as an identity as long as the given predicate holds, then terminate
-- returning the unconsumed portion of the last chunk.
takeWhile :: MonadStream m => (Word8 -> Bool) -> m B.ByteString B.ByteString u B.ByteString
takeWhile p = go
  where
    go = tryAwait >>= maybe (return B.empty) split
    split chunk = do
      let (chunk', leftover) = B.span p chunk
      unless (B.null chunk) $ yield chunk'
      if B.null leftover
        then go
        else return leftover

-- | Drop bytes as long as the given predicate holds, then act as an identity.
dropWhile :: MonadStream m => (Word8 -> Bool) -> m B.ByteString B.ByteString r r
dropWhile p = do
  leftover <- takeWhile (not . p) >+> discard
  yield leftover
  idP

-- | Split the chunked input stream into lines, and yield them individually.
lines :: MonadStream m => m B.ByteString B.ByteString r r
lines = go B.empty
  where
    go leftover = do
      mchunk <- tryAwait
      case mchunk of
        Nothing | B.null leftover -> idP
        Nothing -> yield leftover >> idP
        Just chunk -> split chunk leftover
    split chunk leftover
      | B.null chunk = go leftover
      | B.null rest  = go (mappend leftover chunk)
      | otherwise    = yield (mappend leftover line) >>
                       split (B.drop 1 rest) mempty
      where (line, rest) = B.breakByte 10 chunk

-- | Yield individual bytes of the chunked input stream.
bytes :: MonadStream m => m B.ByteString Word8 r r
bytes = forP $ B.foldl (\p c -> p >> yield c) (return ())
