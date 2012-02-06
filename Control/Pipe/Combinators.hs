-- | Basic pipe combinators.
module Control.Pipe.Combinators (
  ($$),
  fromList,
  nullP,
  fold,
  consume,
  take,
  drop,
  pipeList,
  until,
  groupBy,
  filter,
  ) where

import Control.Monad
import Control.Pipe
import Data.Maybe
import Prelude hiding (until, take, drop, concatMap, filter)

-- | Connect producer to consumer, ignoring producer return value.
infixr 5 $$
($$) :: Monad m => Producer a m r' -> Consumer a m r -> m (Maybe r)
p1 $$ p2 = runPipe $ (p1 >> return Nothing) >+> fmap Just p2

-- | Successively yield elements of a list.
fromList :: Monad m => [a] -> Pipe x a m ()
fromList = mapM_ yield

-- | A pipe that terminates immediately.
nullP :: Monad m => Pipe a b m ()
nullP = return ()

-- | A fold pipe. Apply a binary function to successive input values and an
-- accumulator, and return the final result.
fold :: Monad m => (b -> a -> b) -> b -> Pipe a x m b
fold f z = go z
  where
    go x = tryAwait >>= maybe (return x) (go . f x)

-- | Accumulate all input values into a list.
consume :: Monad m => Pipe a x m [a]
consume = fold (\xs x -> xs . (x:)) id >>= \xs -> return (xs [])

-- | Act as an identity for the first 'n' values, then terminate.
take :: Monad m => Int -> Pipe a a m ()
take n = replicateM_ n $ await >>= yield

-- | Remove the first 'n' values from the stream, then act as an identity.
drop :: Monad m => Int -> Pipe a a m r
drop n = replicateM n await >> idP

-- | Apply a function with multiple return values to the stream.
pipeList :: Monad m => (a -> [b]) -> Pipe a b m r
pipeList f = forever $ await >>= mapM_ yield . f

-- | Terminate as soon as an input satisfying the predicate is received.
until :: Monad m => (a -> Bool) -> Pipe a a m ()
until p = go
  where
    go = await >>= \x -> if p x then return () else yield x >> go

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
      until isNothing >+>
      pipe fromJust >+>
      (consume >>= yield)

-- | Remove values from the stream that don't satisfy the given predicate.
filter :: Monad m => (a -> Bool) -> Pipe a a m r
filter p = forever $ until (not . p)
