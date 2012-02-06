import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Pipe
import Data.Char
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

instance Show (a -> b) where
  show f = "<function>"

foldP :: Monad m => (a -> b -> a) -> a -> Consumer b m a
foldP f z = go z
  where go z = tryAwait >>= maybe (return z) (go . f z)

id' :: Monad m => m r -> Pipe a a m r
id' m = tryAwait >>= maybe (lift m) (\x -> yield x >> id' m)

prop_fold :: (Int -> Int -> Int) -> Int -> [Int] -> Bool
prop_fold f z xs = foldl f z xs == runIdentity (runPipe p)
  where p = (mapM_ yield xs >> return 0) >+> foldP f z

prop_id_finalizer :: String -> Int -> Bool
prop_id_finalizer s n = runWriter (runPipe p) == (s, [n])
  where
    p = (return "") >+>
        (id' (tell [n] >> return s))

prop_id :: Int -> Bool
prop_id n = runIdentity (runPipe p) == Just n
  where
    p = (yield n >> return Nothing) >+>
        id' (return Nothing) >+>
        (await >>= return . Just)

main = defaultMain $ [
  testGroup "properties" $
    [ testProperty "fold" prop_fold
    , testProperty "id_finalizer" prop_id_finalizer
    , testProperty "identity" prop_id
    ]
  ]
