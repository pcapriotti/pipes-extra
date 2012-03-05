{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Typeable
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import qualified Control.Exception as E
import Control.Exception (Exception, IOException, throwTo)
import Control.Monad
import Control.Monad.Trans
import Control.Pipe
import Control.Pipe.Combinators
import Control.Pipe.Exception
import qualified Control.Pipe.Binary as PB
import System.IO
import Prelude hiding (catch)

-- line-by-line reader with verbose initializer and finalizer
reader :: FilePath -> Producer B.ByteString IO ()
reader fp = fReader >+> PB.lines
  where
    fReader = bracket open close PB.handleReader
    open = do
      putStrLn $ "opening file " ++ show fp ++ " for reading"
      openFile fp ReadMode
    close h = do
      hClose h
      putStrLn $ "closed file " ++ show fp

-- line-by-line writer with verbose initializer and finalizer
writer :: FilePath -> Consumer B.ByteString IO ()
writer fp = pipe (`BC.snoc` '\n') >+> fWriter
  where
    fWriter = await >>= \x -> feed x (bracket open close PB.handleWriter)
    open = do
      putStrLn $ "opening file " ++ show fp ++ " for writing"
      openFile fp WriteMode
    close h = do
      hClose h
      putStrLn $ "closed file " ++ show fp

-- interactive pipe
prompt :: Pipe String String IO ()
prompt = forever $ await >>= \q -> do
  lift $ putStr $ q ++ ": "
  r <- lift getLine
  yield r

-- copy "/etc/motd" to "/tmp/x"
ex1 :: Pipeline IO ()
ex1 = reader "/etc/motd" >+> writer "/tmp/x"
{-
  opening file "/etc/motd" for reading
  opening file "/tmp/x" for writing
  closed file "/etc/motd"
  closed file "/tmp/x"
-}
-- note that the files are not closed in LIFO order

-- output error
ex2 :: Pipeline IO ()
ex2 = reader "/etc/motd" >+> writer "/unopenable"
{-
  opening file "/etc/motd" for reading
  opening file "/unopenable" for writing
  closed file "/etc/motd"
  *** Exception: /unopenable: openFile: permission denied (Permission denied)
-}
-- note that the input file was automatically closed before the exception
-- terminated the pipeline

-- joining two files
ex3 :: Pipeline IO ()
ex3 = (reader "/etc/motd" >> reader "/usr/share/dict/words") >+>
      writer "/tmp/x"
{-
  opening file "/etc/motd" for reading
  opening file "/tmp/x" for writing
  closed file "/etc/motd"
  opening file "/usr/share/dict/words" for reading
  closed file "/usr/share/dict/words"
  closed file "/tmp/x"
-}

-- recovering from exceptions
ex4 :: Pipeline IO ()
ex4 = (safeReader "/etc/motd" >> safeReader "/nonexistent") >+>
      writer "/tmp/x"
  where
    safeReader fp = catch (reader fp) $ \(e :: IOException) ->
      lift $ putStrLn $ "exception " ++ show e
{-
  opening file "/etc/motd" for reading
  opening file "/tmp/x" for writing
  closed file "/etc/motd"
  opening file "/nonexistent" for reading
  exception /nonexistent: openFile: does not exist (No such file or directory)
  closed file "/tmp/x"
-}

data Timeout = Timeout
  deriving (Show, Typeable)
instance Exception Timeout

-- recovering from asynchronous exceptions
ex5 :: Pipeline IO ()
ex5 = questions >+> safePrompt >+> pipe BC.pack >+> writer "/tmp/x"
  where
    questions = do
      yield "Project name"
      yield "Version"
      yield "Description"
      lift $ E.throwIO Timeout
    timeout t = lift $ do
      tid <- myThreadId
      forkIO $ do
        threadDelay (t * 1000000)
        throwTo tid Timeout
    safePrompt = catch (timeout 5 >> prompt) $ \(_ :: Timeout) ->
      lift $ putStrLn "timeout"
{-
  Project name: test
  opening file "/tmp/x" for writing
  Version: timeout
  closed file "/tmp/x"
-}
