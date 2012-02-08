module Control.Pipe.Binary (
  -- ** Handle and File IO
  fileReader,
  handleReader,
  handleIOReader,
  fileWriter,
  handleWriter,
  handleIOWriter,

  -- ** Chunked Byte Stream Manipulation
  take,
  takeWhile,
  dropWhile,
  lines,
  bytes,
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Pipe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Monoid
import Data.Word
import System.IO
import Prelude hiding (take, takeWhile, dropWhile, lines)

-- | Read data from a file.
fileReader :: ResourceIO m => FilePath -> Pipe x B.ByteString (ResourceT m) ()
fileReader path = handleIOReader $ openFile path ReadMode

-- | Read data from an open handle.
handleReader :: MonadIO m => Handle -> Pipe x B.ByteString m ()
handleReader h = go
  where
    go = do
      eof <- lift . liftIO $ hIsEOF h
      if eof
        then return ()
        else do
          chunk <- lift . liftIO $ B.hGetSome h 4096
          yield chunk
          go

-- | Read data from an handle, given an IO action to open it.
handleIOReader :: ResourceIO m => IO Handle -> Pipe x B.ByteString (ResourceT m) ()
handleIOReader openHandle = do
  (releaseKey, h) <- lift $ withIO openHandle hClose
  handleReader h
  lift $ release releaseKey

-- | Write data to a file.
--
-- The file is only opened if some data arrives into the pipe.
fileWriter :: ResourceIO m => FilePath -> Pipe B.ByteString x (ResourceT m) ()
fileWriter path = handleIOWriter (openFile path WriteMode)

-- | Write data to a handle.
handleWriter:: MonadIO m => Handle -> Pipe B.ByteString x m ()
handleWriter h = go
  where
    go = do
      input <- tryAwait
      case input of
        Nothing -> return ()
        Just chunk -> do
          lift . liftIO $ B.hPut h chunk
          go

-- | Write data to a handle, given an IO action to open it.
--
-- The handle is only opened if some data arrives into the pipe.
handleIOWriter :: ResourceIO m => IO Handle -> Pipe B.ByteString x (ResourceT m) ()
handleIOWriter openHandle = do
  -- receive some data before opening the handle
  input <- await
  -- feed it back to the stricter version of this consumer
  (yield input >> idP) >+> handleIOWriter' openHandle

-- strict version of handleIOWriter: open handle immediately and write data to it.
handleIOWriter' :: ResourceIO m => IO Handle -> Pipe B.ByteString x (ResourceT m) ()
handleIOWriter' openHandle = do
  (releaseKey, h) <- lift $ withIO openHandle hClose
  handleWriter h
  lift $ release releaseKey

-- | Act as an identity for the first 'n' bytes, then terminate returning the
-- unconsumed portion of the last chunk.
take :: Monad m => Int -> Pipe B.ByteString B.ByteString m B.ByteString
take n = go n
  where
    go size = do
      chunk <- await
      let (chunk', leftover) = B.splitAt size chunk
      yield chunk'
      if B.null leftover
        then go $ size - B.length chunk'
        else return leftover

-- | Act as an identity as long as the given predicate holds, then terminate
-- returning the unconsumed portion of the last chunk.
takeWhile :: Monad m => (Word8 -> Bool) -> Pipe B.ByteString B.ByteString m B.ByteString
takeWhile p = go
  where
    go = do
      chunk <- await
      let (chunk', leftover) = B.span p chunk
      when (not $ B.null chunk) $ yield chunk'
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
