import Criterion.Main
import Control.Monad.IO.Class
import Control.Pipe
import System.IO

printer :: Show a => Handle -> Pipe IO a x r r
printer h = forP $ liftIO . hPrint h

devnull :: IO Handle
devnull = openFile "/dev/null" WriteMode

main :: IO ()
main = do
    h <- devnull
    defaultMain
      [ bench "list >+> printer (1000)" (whnfIO $ runPipe $ mapM_ yield [1..1000 :: Int] >+> printer h)
      , bench "mapM_ print (1000)" (whnfIO $ mapM_ (hPrint h) [1..1000 :: Int])
      , bench "list >+> printer (100000)" (whnfIO $ runPipe $ mapM_ yield [1..100000 :: Int] >+> printer h)
      , bench "mapM_ print (100000)" (whnfIO $ mapM_ (hPrint h . show) [1..100000 :: Int])
      ]
