module Control.Pipe.Binary (
  fileProducer,
  handleProducer,
  handleIOProducer,
  fileConsumer,
  handleConsumer,
  handleIOConsumer,
  isolate,
  takeWhile,
  dropWhile,
  lines,
  bytes,
  ) where

-- adapted from conduit

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Pipe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Monoid
import Data.Word
import System.IO
import Prelude hiding (takeWhile, dropWhile, lines)

fileProducer :: ResourceIO m => FilePath -> Producer B.ByteString (ResourceT m) ()
fileProducer path = handleIOProducer $ openFile path ReadMode

handleProducer :: MonadIO m => Handle -> Producer B.ByteString m ()
handleProducer h = go
  where
    go = do
      eof <- lift . liftIO $ hIsEOF h
      if eof
        then return ()
        else do
          chunk <- lift . liftIO $ B.hGetSome h 4096
          yield chunk
          go

handleIOProducer :: ResourceIO m => IO Handle -> Producer B.ByteString (ResourceT m) ()
handleIOProducer openHandle = do
  (releaseKey, h) <- lift $ withIO openHandle hClose
  handleProducer h
  lift $ release releaseKey

fileConsumer :: ResourceIO m => FilePath -> Consumer B.ByteString (ResourceT m) ()
fileConsumer path = handleIOConsumer (openFile path WriteMode)

handleConsumer :: MonadIO m => Handle -> Consumer B.ByteString m ()
handleConsumer h = go
  where
    go = do
      input <- tryAwait
      case input of
        Nothing -> return ()
        Just chunk -> do
          lift . liftIO $ B.hPut h chunk
          go

handleIOConsumer :: ResourceIO m => IO Handle -> Consumer B.ByteString (ResourceT m) ()
handleIOConsumer openHandle = do
  -- receive some data before opening the handle
  input <- await
  -- feed it back to the stricter version of this consumer
  (yield input >> idP) >+> handleIOConsumer' openHandle

handleIOConsumer' :: ResourceIO m => IO Handle -> Consumer B.ByteString (ResourceT m) ()
handleIOConsumer' openHandle = do
  (releaseKey, h) <- lift $ withIO openHandle hClose
  handleConsumer h
  lift $ release releaseKey

isolate :: Monad m => Int -> Pipe B.ByteString B.ByteString m B.ByteString
isolate n = go n
  where
    go size = do
      chunk <- await
      let (chunk', leftover) = B.splitAt size chunk
      yield chunk'
      if B.null leftover
        then go $ size - B.length chunk'
        else return leftover

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

dropWhile :: Monad m => (Word8 -> Bool) -> Pipe B.ByteString B.ByteString m r
dropWhile p = do
  leftover <- takeWhile (not . p) >+> discard
  yield leftover
  idP

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

bytes :: Monad m => Pipe B.ByteString Word8 m r
bytes = forever $ await >>= B.foldl (\p c -> p >> yield c) (return ())
