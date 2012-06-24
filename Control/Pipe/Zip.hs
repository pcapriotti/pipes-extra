module Control.Pipe.Zip (
  controllable,
  controllable_,
  zip,
  zip_,
  ProducerControl(..),
  ZipControl(..),
  ) where

import qualified Control.Exception as E
import Control.Monad
import Control.Pipe
import Control.Pipe.Class
import Control.Pipe.Coroutine
import Control.Pipe.Exception
import Prelude hiding (zip)

data ProducerControl r
  = Done r
  | Error E.SomeException

controllable :: Monad m
             => Producer m a u r
             -> Pipe m (Either () (ProducerControl r)) a u r
controllable p = do
  x <- pipe (const ()) >+> suspend p
  case x of
    Left r -> return r
    Right (b, p') ->
      join $ onException
        (awaitE >>= \c -> case c of
          Left u -> return $ controllable (return u >+> resume p')
          Right c' -> case c' of
            Left () -> yield b >> return (controllable (resume p'))
            Right (Done r) -> return $ (pipe (const ()) >+> terminate p') >> return r
            Right (Error e) -> return $ (pipe (const ()) >+> terminate p') >> throw e)
        (pipe (const ()) >+> terminate p')

controllable_ :: Monad m
              => Producer m a u r
              -> Producer m a u r
controllable_ p = pipe Left >+> controllable p

data ZipControl r s
  = LeftZ (ProducerControl r)
  | RightZ (ProducerControl s)

zip :: Monad m
    => Producer m a u r
    -> Producer m b r s
    -> Pipe m (Either () (ZipControl r s)) (Either a b) u s
zip p1 p2 = translate >+> (controllable p1 *+* controllable p2)
  where
    translate = withDefer . forever $ await >>= \x -> case x of
      Left () -> (yield . Left . Left $ ()) >> (yield . Right . Left $ ())
      Right (LeftZ c) -> (yield . Left . Right $ c) >> (yield . Right . Left $ ())
      Right (RightZ c) -> (yield . Left . Left $ ()) >> (yield . Right . Right $ c)

zip_ :: Monad m
     => Producer m a u r
     -> Producer m b r s
     -> Producer m (Either a b) u s
zip_ p1 p2 = pipe Left >+> zip p1 p2

(*+*) :: Monad m
      => Pipe m a b u r
      -> Pipe m a' b' r s
      -> Pipe m (Either a a') (Either b b') u s
p1 *+* p2 = p1' >+> p2'
  where
    p1' = do
      r <- firstP p1 >+> pipe (either Left (Right . Right))
      forever . yield . Right . Left $ r
    p2' = secondP $ continue >+> p2

    continue = awaitE >>= \c -> case c of
      Left r -> return r
      Right (Left r) -> return r
      Right (Right x) -> yield x >> continue
