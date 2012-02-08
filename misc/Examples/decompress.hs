import Control.Monad.Trans.Resource
import Control.Pipe
import Control.Pipe.Binary
import Control.Pipe.Zlib
import System.IO

main :: IO ()
main = runResourceT . runPipe $
  handleProducer stdin >+> ungzip >+> handleConsumer stdout
