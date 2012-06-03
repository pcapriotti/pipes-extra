import Criterion.Main

import Control.Monad
import Control.Monad.Trans.Class
import Control.Pipe
import System.IO

printer :: Show a => Handle -> Pipe a x IO r
printer h = forever $ await >>= lift . hPutStrLn h . show

devnull :: IO Handle
devnull = openFile "/dev/null" WriteMode

main :: IO ()
main = do
    h <- devnull
    defaultMain
      [ bench "list >+> printer (1000)" (whnfIO $ runPipe $ mapM_ yield [1..1000 :: Int] >+> printer h)
      , bench "mapM_ print (1000)" (whnfIO $ mapM_ (hPutStrLn h . show) [1..1000 :: Int])
      , bench "list >+> printer (100000)" (whnfIO $ runPipe $ mapM_ yield [1..100000 :: Int] >+> printer h)
      , bench "mapM_ print (100000)" (whnfIO $ mapM_ (hPutStrLn h . show) [1..100000 :: Int])
      ]

