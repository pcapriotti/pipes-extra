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
import Control.Pipe.Coroutine
import Control.Pipe.Exception
import Prelude hiding (zip)

data ProducerControl r
  = Done r
  | Error E.SomeException

controllable :: Monad m
             => Producer Void a m r
             -> Pipe l (Either () (ProducerControl r)) a m r
controllable p = do
  x <- pipe (const ()) >+> suspend p
  case x of
    Left r -> return r
    Right (b, p') ->
      join $ onException
        (await >>= \c -> case c of
          Left () -> yield b >> return (controllable (resume p'))
          Right (Done r) -> return $ (pipe (const ()) >+> terminate p') >> return r
          Right (Error e) -> return $ (pipe (const ()) >+> terminate p') >> throw e)
        (pipe (const ()) >+> terminate p')

controllable_ :: Monad m
              => Producer Void a m r
              -> Producer l a m r
controllable_ p = pipe Left >+> controllable p

data ZipControl r
  = LeftZ (ProducerControl r)
  | RightZ (ProducerControl r)

zip :: Monad m
    => Producer Void a m r
    -> Producer Void b m r
    -> Pipe l (Either () (ZipControl r)) (Either a b) m r
zip p1 p2 = translate >+> (controllable p1 *+* controllable p2)
  where
    translate = forever $ await >>= \z -> case z of
      Left () -> (yield . Left . Left $ ()) >> (yield . Right . Left $ ())
      Right (LeftZ c) -> (yield . Left . Right $ c) >> (yield . Right . Left $ ())
      Right (RightZ c) -> (yield . Left . Left $ ()) >> (yield . Right . Right $ c)

zip_ :: Monad m
     => Producer Void a m r
     -> Producer Void b m r
     -> Producer l (Either a b) m r
zip_ p1 p2 = pipe Left >+> zip p1 p2

(*+*) :: Monad m
      => Pipe Void a b m r
      -> Pipe Void a' b' m r
      -> Pipe Void (Either a a') (Either b b') m r
p1 *+* p2 = (continue p1 *** continue p2) >+> both
  where
    continue p = do
      r <- p >+> pipe Right
      yield $ Left r
      discard
    both = await >>= \x -> case x of
      Left c -> either (const right) (\a -> yield (Left a) >> both) c
      Right c -> either (const left) (\b -> yield (Right b) >> both) c
    left = await >>= \x -> case x of
      Left c -> either return (\a -> yield (Left a) >> left) c
      Right _ -> left
    right = await >>= \x -> case x of
      Left _ -> right
      Right c -> either return (\b -> yield (Right b) >> right) c
