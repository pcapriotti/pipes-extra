import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer (tell, runWriter)
import Control.Pipe
import Control.Pipe.Combinators (($$))
import qualified Control.Pipe.Combinators as P
import Data.Char
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

instance Show (a -> b) where
  show f = "<function>"

id' :: Monad m => m r -> Pipe a a m r
id' m = tryAwait >>= maybe (lift m) (\x -> yield x >> id' m)

prop_fold :: (Int -> Int -> Int) -> Int -> [Int] -> Bool
prop_fold f z xs = foldl f z xs == runIdentity (runPipe p)
  where p = (mapM_ yield xs >> return 0) >+> P.fold f z

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

run :: Pipe () Void Identity r -> r
run = runIdentity . runPipe

prop_consume :: [Int] -> Bool
prop_consume xs =
  run (P.fromList xs $$ P.consume) ==
  Just xs

prop_take :: Int -> [Int] -> Bool
prop_take n xs =
  run (P.fromList xs >+> P.take n $$ P.consume) ==
  Just (take n xs)

prop_take_head :: Int -> [Int] -> Bool
prop_take_head n xs =
  run (P.fromList xs >+> P.take (n + 1) $$ await) ==
  run (P.fromList xs $$ await)

prop_drop :: Int -> [Int] -> Bool
prop_drop n xs =
  run (P.fromList xs >+> P.drop n $$ P.consume)
  == Just (drop n xs)

prop_pipeList :: [Int] -> (Int -> [Int]) -> Bool
prop_pipeList xs f =
  run (P.fromList xs >+> P.pipeList f $$ P.consume) ==
  Just (concatMap f xs)

prop_takeWhile :: (Int -> Bool) -> [Int] -> Bool
prop_takeWhile p xs =
  run (P.fromList xs >+> P.takeWhile_ p $$ P.consume) ==
  Just (takeWhile p xs)

prop_dropWhile:: (Int -> Bool) -> [Int] -> Bool
prop_dropWhile p xs =
  run (P.fromList xs >+> P.dropWhile p $$ P.consume) ==
  Just (dropWhile p xs)

prop_groupBy :: [Int] -> Bool
prop_groupBy xs =
  run (P.fromList xs >+> P.groupBy (==) $$ P.consume) ==
  Just (groupBy (==) xs)

prop_filter :: [Int] -> (Int -> Bool) -> Bool
prop_filter xs p =
  run (P.fromList xs >+> P.filter p $$ P.consume) ==
  Just (filter p xs)

main = defaultMain $ [
  testGroup "properties" $
    [ testProperty "fold" prop_fold
    , testProperty "id_finalizer" prop_id_finalizer
    , testProperty "identity" prop_id
    , testProperty "consume . fromList" prop_consume
    , testProperty "take . fromList" prop_take
    , testProperty "head . take == head" prop_take
    , testProperty "drop . fromList" prop_take
    , testProperty "pipeList == concatMap" prop_pipeList
    , testProperty "takeWhile" prop_takeWhile
    , testProperty "dropWhile" prop_dropWhile
    , testProperty "groupBy" prop_groupBy
    , testProperty "filter" prop_filter
    ]
  ]
