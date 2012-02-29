-- | Basic pipe combinators.
module Control.Pipe.Combinators (
  ($$),
  fromList,
  nullP,
  fold,
  fold1,
  consume,
  consume1,
  take,
  drop,
  pipeList,
  takeWhile,
  takeWhile_,
  dropWhile,
  intersperse,
  groupBy,
  filter,
  feed,
  ) where

import Control.Applicative
import Control.Monad
import Control.Pipe
import Data.Maybe
import Prelude hiding (until, take, drop, concatMap, filter, takeWhile, dropWhile)

-- | Connect producer to consumer, ignoring producer return value.
infixr 5 $$
($$) :: Monad m => Pipe x a m r' -> Pipe a y m r -> Pipe x y m (Maybe r)
p1 $$ p2 = (p1 >> return Nothing) >+> fmap Just p2

-- | Successively yield elements of a list.
fromList :: Monad m => [a] -> Pipe x a m ()
fromList = mapM_ yield

-- | A pipe that terminates immediately.
nullP :: Monad m => Pipe a b m ()
nullP = return ()

-- | A fold pipe. Apply a binary function to successive input values and an
-- accumulator, and return the final result.
fold :: Monad m => (b -> a -> b) -> b -> Pipe a x m b
fold f = go
  where
    go x = tryAwait >>= maybe (return x) (go . f x)

-- | A variation of 'fold' without an initial value for the accumulator. This
-- pipe doesn't return any value if no input values are received.
fold1 :: Monad m => (a -> a -> a) -> Pipe a x m a
fold1 f = tryAwait >>= maybe discard (fold f)

-- | Accumulate all input values into a list.
consume :: Monad m => Pipe a x m [a]
consume = pipe (:) >+> (fold (.) id <*> pure [])

-- | Accumulate all input values into a non-empty list.
consume1 :: Monad m => Pipe a x m [a]
consume1 = pipe (:) >+> (fold1 (.) <*> pure [])

-- | Act as an identity for the first 'n' values, then terminate.
take :: Monad m => Int -> Pipe a a m ()
take n = replicateM_ n $ await >>= yield

-- | Remove the first 'n' values from the stream, then act as an identity.
drop :: Monad m => Int -> Pipe a a m r
drop n = replicateM_ n await >> idP

-- | Apply a function with multiple return values to the stream.
pipeList :: Monad m => (a -> [b]) -> Pipe a b m r
pipeList f = forever $ await >>= mapM_ yield . f

-- | Act as an identity until as long as inputs satisfy the given predicate.
-- Return the first element that doesn't satisfy the predicate.
takeWhile :: Monad m => (a -> Bool) -> Pipe a a m a
takeWhile p = go
  where
    go = await >>= \x -> if p x then yield x >> go else return x

-- | Variation of 'takeWhile' returning @()@.
takeWhile_ :: Monad m => (a -> Bool) -> Pipe a a m ()
takeWhile_ p = takeWhile p >> return ()

-- | Remove inputs as long as they satisfy the given predicate, then act as an
-- identity.
dropWhile :: Monad m => (a -> Bool) -> Pipe a a m r
dropWhile p = (takeWhile p >+> discard) >>= yield >> idP

-- | Yield Nothing when an input satisfying the predicate is received.
intersperse :: Monad m => (a -> Bool) -> Pipe a (Maybe a) m r
intersperse p = forever $ do
  x <- await
  when (p x) $ yield Nothing
  yield $ Just x

-- | Group input values by the given predicate.
groupBy :: Monad m => (a -> a -> Bool) -> Pipe a [a] m r
groupBy p = streaks >+> createGroups
  where
    streaks = await >>= \x -> yield (Just x) >> streaks' x
    streaks' x = do
      y <- await
      unless (p x y) $ yield Nothing
      yield $ Just y
      streaks' y
    createGroups = forever $
      takeWhile_ isJust >+>
      pipe fromJust >+>
      (consume1 >>= yield)

-- | Remove values from the stream that don't satisfy the given predicate.
filter :: Monad m => (a -> Bool) -> Pipe a a m r
filter p = forever $ takeWhile_ p

-- | Feed an input element to a pipe.
feed :: Monad m => a -> Pipe a b m r -> Pipe a b m r

-- this could be implemented as
-- feed x p = (yield x >> idP) >+> p
-- but this version is more efficient
feed _ (Pure r) = return r
feed _ (Throw e) = throw e
feed a (Free c h) = case go a c of
  (False, p) -> p >>= feed a
  (True, p)  -> join p
  where
    go a (Await k) = (True, return $ k a)
    go _ (Yield y c) = (False, yield y >> return c)
    go _ (M m s) = (False, lift_ s m)
