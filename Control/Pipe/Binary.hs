{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Trans.Class
import Control.Pipe
import Control.Pipe.Exception
import Control.Pipe.Combinators (tryAwait, feed)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Monoid
import Data.Word
import System.IO
import Prelude hiding (take, takeWhile, dropWhile, lines, catch)

-- | Read data from a file.
fileReader :: MonadIO m => FilePath -> Pipe () B.ByteString m ()
fileReader path = bracket
  (liftIO $ openFile path ReadMode)
  (liftIO . hClose)
  handleReader

-- | Read data from an open handle.
handleReader :: MonadIO m => Handle -> Pipe () B.ByteString m ()
handleReader h = go
  where
    go = do
      eof <- lift . liftIO $ hIsEOF h
      unless eof $ do
        chunk <- lift . liftIO $ B.hGetSome h 4096
        yield chunk
        go

-- | Write data to a file.
--
-- The file is only opened if some data arrives into the pipe.
fileWriter :: MonadIO m => FilePath -> Pipe B.ByteString Void m ()
fileWriter path = do
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
handleWriter:: MonadIO m => Handle -> Pipe B.ByteString Void m ()
handleWriter h = forever $ do
  chunk <- await
  lift . liftIO . B.hPut h $ chunk

-- | Act as an identity for the first 'n' bytes, then terminate returning the
-- unconsumed portion of the last chunk.
take :: Monad m => Int -> Pipe B.ByteString B.ByteString m B.ByteString
take size = do
  chunk <- await
  let (chunk', leftover) = B.splitAt size chunk
  yield chunk'
  if B.null leftover
    then take $ size - B.length chunk'
    else return leftover

-- | Act as an identity as long as the given predicate holds, then terminate
-- returning the unconsumed portion of the last chunk.
takeWhile :: Monad m => (Word8 -> Bool) -> Pipe B.ByteString B.ByteString m B.ByteString
takeWhile p = go
  where
    go = do
      chunk <- await
      let (chunk', leftover) = B.span p chunk
      unless (B.null chunk) $ yield chunk'
      if B.null leftover
        then go
        else return leftover

-- | Drop bytes as long as the given predicate holds, then act as an identity.
dropWhile :: Monad m => (Word8 -> Bool) -> Pipe B.ByteString B.ByteString m r
dropWhile p = do
  leftover <- takeWhile (not . p) >+> discard
  yield leftover
  idP

-- | Split the chunked input stream into lines, and yield them individually.
lines :: Monad m => Pipe B.ByteString B.ByteString m r
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
bytes :: Monad m => Pipe B.ByteString Word8 m r
bytes = forever $ await >>= B.foldl (\p c -> p >> yield c) (return ())
