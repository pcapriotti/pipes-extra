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

import Control.Monad.IO.Class
import Control.Pipe
import Control.Pipe.Binary
import Data.ByteString

-- | Acts like 'idP', but also passes a copy to the supplied consumer.
tee :: (Monad m)
    => Pipe m a Void u r -- ^ 'Consumer' that will receive a copy of all the input
    -> Pipe m a a u r
tee consumer =
    splitP >+> firstP consumer >+> discardL

-- | Acts like 'idP', but also writes a copy to a file.
teeFile :: (MonadIO m)
        => (a -> ByteString)   -- ^ function to convert the value to a
                               --   'ByteString' which can be written to
                               --   the log
        -> FilePath            -- ^ file to log to
        -> Pipe m a a r r
teeFile showBS logFile =
    tee (pipe showBS >+> fileWriter logFile)

-- | Acts like, 'idP', but also writes a copy to the specified log file.
--
-- This function is equivalent to @teeFile id@.
teeFileBS :: (MonadIO m)
          => FilePath                         -- ^ file to log to
          -> Pipe m ByteString ByteString r r
teeFileBS = teeFile id
