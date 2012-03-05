import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad
import Control.Monad.Trans
import Control.Pipe
import qualified Control.Pipe.Binary as PB
import Control.Pipe.Combinators
import Control.Pipe.Exception
import Control.Pipe.Zip
import System.IO
import Prelude hiding (filter, zip)

import Control.Pipe.Coroutine

-- line-by-line reader with verbose initializer and finalizer
reader :: FilePath -> Producer B.ByteString IO ()
reader fp = fReader >+> PB.lines >+> filter (not . B.null)
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

-- Oleg's motivating example for monadic regions, reimplemented with Pipes.
--
-- 1. open two files for reading, one of them a configuration file
-- 2. read the name of an output file from the configuration file
-- 3. open the output file and zip the contents of both input files into
--    the output file
-- 4. close the configuration file
-- 5. copy the rest, if any, of the other input file to the output file
ex1 :: Pipeline IO ()
ex1 = reader "conf" >+>       -- read configuration file
      (await >>= process)     -- get first line and pass it to process
  where
    process out =
          continue                  -- keep running when conf terminates
      >+> splitP                    -- create second channel
      >+> (   justs                 -- discard Nothing values on the first channel
          *** reader' "input.txt")  -- get input file on the second channel
      >+> joinP                     -- merge input streams
      >+> writer (BC.unpack out)    -- save to output file

    continue = forP (yield . Just) >> forever (yield Nothing)
    justs = forever $ await >>= maybe (return ()) yield
    reader' fp = pipe (const ()) >+> controllable_ (reader fp)

-- Another example, demonstrating the use of controllable producers.
--
-- 1. open two input files
-- 2. read one number out of each input file
-- 3. close the input file with the lowest number
-- 4. continue processing the other input file
-- 5. close the other input file
ex2 :: Pipeline IO ()
ex2 = go (reader "input.txt") (reader "input2.txt") >+> printer
  where
    go p1 p2 = loopP $ zip p1 p2 >+> choose
    choose = do
      (Left line1) <- await
      (Right line2) <- await
      let n1 = read (BC.unpack line1) :: Int
      let n2 = read (BC.unpack line2) :: Int
      -- close file with the lowest number
      if n1 < n2
        then yield (Right (LeftZ (Done ())))
        else yield (Right (RightZ (Done ())))
      -- continue processing the other file
      joinP >+> pipe Left

printer :: Show a => Pipe a Void IO r
printer = forever $ await >>= lift . print
