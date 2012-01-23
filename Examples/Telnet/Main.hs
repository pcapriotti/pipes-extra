import Control.Concurrent (forkIO, killThread)
import Control.Monad.State
import Control.Monad.Trans.Resource
import Control.Pipe.Guarded
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

hConnect :: Handle -> Handle -> ResourceT IO ()
hConnect h1 h2 = runPipe $ handleProducer h1 >+> handleConsumer h2

telnet :: String -> Int -> IO ()
telnet host port = runResourceT $ do
    (releaseSock, hsock) <- with (connectTo host $ PortNumber $ fromIntegral port) hClose
    liftIO $ mapM_ (`hSetBuffering` LineBuffering) [ stdin, stdout, hsock ]
    (releaseThread, _) <- with (
                          forkIO . runResourceT $ hConnect stdin hsock
                          ) killThread
    hConnect hsock stdout
