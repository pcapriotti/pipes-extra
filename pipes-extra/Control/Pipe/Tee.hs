{- |
Module      :  Control.Pipe.Tee
Description :  Combinators which act like 'idP' but also divert a copy of the input to a 'Consumer', similar to the unix 'tee' command.
Copyright   :  (c) 2012 Jeremy Shaw
License     :  BSD3

Maintainer  :  jeremy@n-heptane.com
Stability   :  experimental
Portability :  portable

The 'tee' combinators act like 'idP', but also send a copy of the input to the supplied 'Consumer', a file, etc. This is typically done for the purpose of logging a pipeline, showing progress, etc.
-}
module Control.Pipe.Tee where

import Control.Monad.Trans   (MonadIO(..))
import Control.Pipe          (Pipe, (>+>), firstP, discardL, idP, pipe)
import Control.Pipe.Binary   (fileWriter)
import Control.Pipe.Monoidal ((***), splitP)
import Data.ByteString       (ByteString)
import Data.Void             (Void)

-- | acts like 'idP', but also passes a copy to the supplied consumer
tee :: (Monad m) =>
       Pipe a Void m r -- ^ 'Consumer' that will receive a copy of all the input
    -> Pipe a a m r
tee consumer =
    splitP >+> firstP consumer >+> discardL

-- | acts like 'idP', but also writes a copy to a file
teeFile :: (MonadIO m) =>
           (a -> ByteString)   -- ^ function to convert the value to a 'ByteString' which can be written to the log
        -> FilePath -- ^ file to log to
        -> Pipe a a m ()
teeFile showBS logFile =
    tee (pipe showBS >+> fileWriter logFile)

-- | acts like, 'idP', but also writes a copy to the specified log file
--
-- just an alias for @teeFile id@
teeFileBS :: (MonadIO m) =>
           FilePath -- ^ file to log to
        -> Pipe ByteString ByteString m ()
teeFileBS = teeFile id
