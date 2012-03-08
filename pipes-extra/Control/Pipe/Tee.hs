-- |
-- Module      :  Control.Pipe.Tee
-- Copyright   :  (c) 2012 Jeremy Shaw
--                (c) 2012 Paolo Capriotti
-- License     :  BSD3
--
-- Maintainer  :  jeremy@n-heptane.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'tee' combinators act like 'idP', but also send a copy of the input to the
-- supplied 'Consumer' (e.g. a file). This is typically done for the purpose of
-- logging a pipeline, showing progress, etc.
module Control.Pipe.Tee (
  tee,
  teeFile,
  teeFileBS
  ) where

import Control.Monad.Trans   (MonadIO(..))
import Control.Pipe
import Control.Pipe.Binary   (fileWriter)
import Data.ByteString       (ByteString)
import Data.Void             (Void)

-- | Acts like 'idP', but also passes a copy to the supplied consumer.
tee :: (Monad m)
    => Pipe a Void m r -- ^ 'Consumer' that will receive a copy of all the input
    -> Pipe a a m r
tee consumer =
    splitP >+> firstP consumer >+> discardL

-- | Acts like 'idP', but also writes a copy to a file.
teeFile :: (MonadIO m)
        => (a -> ByteString)   -- ^ function to convert the value to a
                               --   'ByteString' which can be written to
                               --   the log
        -> FilePath            -- ^ file to log to
        -> Pipe a a m ()
teeFile showBS logFile =
    tee (pipe showBS >+> fileWriter logFile)

-- | Acts like, 'idP', but also writes a copy to the specified log file.
--
-- This function is equivalent to @teeFile id@.
teeFileBS :: (MonadIO m)
          => FilePath                         -- ^ file to log to
          -> Pipe ByteString ByteString m ()
teeFileBS = teeFile id
