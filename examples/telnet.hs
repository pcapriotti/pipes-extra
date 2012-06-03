import Control.Concurrent (forkIO)
import qualified Control.Exception as E
import Control.Pipe
import Control.Pipe.Binary
import Network (connectTo, PortID (..))
import System.Environment (getArgs, getProgName)
import System.IO


main :: IO ()
main = do
    args <- getArgs
    case args of
        [host, port] -> telnet host (read port :: Int)
        _ -> usageExit
  where
    usageExit = do
        name <- getProgName
        putStrLn $ "Usage : " ++ name ++ " host port"

hConnect :: Handle -> Handle -> IO ()
hConnect h1 h2 = runPipe $ handleReader h1 >+> handleWriter h2

telnet :: String -> Int -> IO ()
telnet host port = E.bracket
    (connectTo host (PortNumber (fromIntegral port)))
    hClose
    (\hsock -> do
      mapM_ (`hSetBuffering` LineBuffering) [ stdin, stdout, hsock ]
      forkIO $ hConnect stdin hsock
      hConnect hsock stdout)
