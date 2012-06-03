import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

import Control.Pipe.Binary
import Control.Pipe.Combinators
import Control.Pipe

testFile :: FilePath
testFile = "bench/general.hs"

main :: IO ()
main = defaultMain
    [ bench "bigsum-pipes" (whnfIO $ runPipe $ (mapM_ yield [1..1000 :: Int] >> return 0) >+> fold (+) 0)
    , bench "bigsum-conduit" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) 0)
    , bench "fileread-pipes" (whnfIO $ runPipe $ fileReader testFile >+> discard)
    , bench "fileread-conduit" (whnfIO $ C.runResourceT $ CB.sourceFile testFile C.$$ CL.sinkNull)
    , bench "map-pipes" (whnfIO $ runPipe $ (mapM_ yield [1..1000 :: Int] >> return 0) >+> pipe (+1) >+> fold (+) 0)
    , bench "map-conduit" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$= CL.map (+ 1) C.$$ CL.fold (+) 0)
    ]
