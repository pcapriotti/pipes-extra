{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.GZip as GZip
import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Codec.Zlib.Enum as EZ
import qualified System.IO as SIO

import Control.Pipe
import qualified Control.Pipe.Binary as PB
import qualified Control.Pipe.Zlib as PZ

filePath :: FilePath
filePath = "bench/sample.gz"

tmpFile :: FilePath
tmpFile = "dist/build/autogen/bench-tmp"

lazyIO :: IO ()
lazyIO = do
    compressed <- L.readFile filePath
    L.writeFile tmpFile $ GZip.decompress compressed

conduits1 :: IO ()
conduits1 = C.runResourceT
    $ CB.sourceFile filePath
    C.$$ CZ.ungzip
    C.=$ CB.sinkFile tmpFile

conduits2 :: IO ()
conduits2 = C.runResourceT
    $ CB.sourceFile filePath
    C.$= CZ.ungzip
    C.$$ CB.sinkFile tmpFile

enumerator :: IO ()
enumerator = SIO.withBinaryFile tmpFile SIO.WriteMode $ \h -> E.run_
    $ EB.enumFile filePath
    E.$$ EZ.ungzip
    E.=$ EB.iterHandle h

pipes :: IO ()
pipes = runPipe
  $ PB.fileReader filePath
  >+> PZ.gunzip
  >+> PB.fileWriter tmpFile

main :: IO ()
main = defaultMain
    [ bench "pipes" pipes
    , bench "conduits1" conduits1
    , bench "conduits2" conduits2
    , bench "enumerator1" enumerator
    , bench "lazyIO" lazyIO
    ]

