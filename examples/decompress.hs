import Control.Pipe
import Control.Pipe.Binary
import Control.Pipe.Zlib
import System.IO

main :: IO ()
main = runPipe $
  handleReader stdin >+> gunzip >+> handleWriter stdout
