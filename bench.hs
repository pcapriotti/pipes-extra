import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

import Control.Pipe.Guarded
import Control.Pipe.Binary

foldP :: Monad m => (a -> b -> a) -> a -> Consumer b m a
foldP f z = go z
  where go z = tryAwait >>= maybe (return z) (go . f z)

fromList :: Monad m => [a] -> Producer a m ()
fromList xs = mapM_ yield xs

main :: IO ()
main = defaultMain
    [ bench "bigsum-pipes" (whnfIO $ C.runResourceT . runPipe $ (fromList [1..1000 :: Int] >> return 0) >+> foldP (+) 0)
    , bench "bigsum" (whnfIO $ C.runResourceT . runPipe $ (fromList [1..1000 :: Int] >> return 0) >+> foldP (+) 0)
    , bench "bigsum-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CL.sourceList [1..1000 :: Int]
        bsrc C.$$ CL.fold (+) 0)
    , bench "fileread-pipes" (whnfIO $ C.runResourceT . runPipe $ fileProducer "bench" >+> discard)
    , bench "fileread" (whnfIO $ C.runResourceT $ CB.sourceFile "bench" C.$$ CL.sinkNull)
    , bench "fileread-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CB.sourceFile "bench"
        bsrc C.$$ CL.sinkNull)
    , bench "map-pipes" (whnfIO $ C.runResourceT . runPipe $ (fromList [1..1000 :: Int] >> return 0) >+> pipe (+1) >+> foldP (+) 0)
    , bench "map" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$= CL.map (+ 1) C.$$ CL.fold (+) 0)
    , bench "map-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CL.sourceList [1..1000 :: Int]
        bsrc C.$= CL.map (+ 1) C.$$ CL.fold (+) 0)
    , bench "map-buffer-alt" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CL.sourceList [1..1000 :: Int] C.$= CL.map (+ 1)
        bsrc C.$$ CL.fold (+) 0)
    ]

