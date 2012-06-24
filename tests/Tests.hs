{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad.Reader hiding (reader)
import Control.Pipe
import Control.Pipe.Class
import Control.Pipe.Combinators
import Control.Pipe.Exception
import qualified Control.Pipe.Binary as PB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List
import Prelude hiding (catch)

import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime

import System.IO

data Action
  = OpenFile FilePath IOMode
  | CloseFile FilePath
  | CaughtException E.IOException
  deriving (Eq, Show)

type Report = IORef [Action]

type M = ReaderT Report IO

runPipeM :: Pipeline M u r -> IO (Either SomeException r, [Action])
runPipeM p = do
  r <- newIORef []
  result <- E.try $ runReaderT (runPipe p) r
  acts <- readIORef r
  return (result, reverse acts)

saveAction :: Action -> M ()
saveAction act = do
  r <- ask
  liftIO . modifyIORef r $ (act:)

open :: FilePath -> IOMode -> M Handle
open fp mode = do
  saveAction (OpenFile fp mode)
  liftIO $ openFile fp mode

close :: FilePath -> Handle -> M ()
close fp h = do
  liftIO $ hClose h
  saveAction (CloseFile fp)

reader :: FilePath -> Producer M ByteString u ()
reader fp = fReader >+> PB.lines
  where
    fReader = bracket
      (open fp ReadMode)
      (close fp)
      PB.handleReader

-- line-by-line writer with verbose initializer and finalizer
writer :: FilePath -> Consumer M ByteString r r
writer fp = pipe (`BC.snoc` '\n') >+> fWriter
  where
    fWriter = withDefer $ do
      x <- await
      feed x $
        bracket
          (open fp WriteMode)
          (close fp)
          PB.handleWriter

equalFiles :: FilePath -> FilePath -> Assertion
equalFiles fp1 fp2 = do
  content1 <- readFile fp1
  content2 <- readFile fp2
  content1 @=? content2

assertLeft :: Show b => Either a b -> (a -> Assertion) -> Assertion
assertLeft x f = either f err x
  where
    err b = assertFailure $ "expected Left, got " ++ show b

assertRight :: Show a => Either a b -> (b -> Assertion) -> Assertion
assertRight x f = either err f x
  where
    err a = assertFailure $ "expected Right, got " ++ show a

tmpOutput :: FilePath
tmpOutput = "dist/build/testtmp"

case_cp :: Assertion
case_cp = do
  let input = "README.md"
  (r, acts) <- runPipeM $ reader input >+> writer tmpOutput
  assertRight r $ \_ -> return ()

  acts @?=
    [ OpenFile input ReadMode
    , OpenFile tmpOutput WriteMode
    , CloseFile input
    , CloseFile tmpOutput ]

  equalFiles input tmpOutput

isNonexistingException :: SomeException -> Assertion
isNonexistingException e =
  "does not exist" `isInfixOf` show e @?
     "expected 'no such file' exception, "
           ++ "got " ++ show e

case_unopenable :: Assertion
case_unopenable = do
  let input = "README.md"
      output = "/unopenable/file"
  (result, acts) <- runPipeM $ reader input >+> writer output
  assertLeft result isNonexistingException

  acts @?=
    [ OpenFile input ReadMode
    , OpenFile output WriteMode
    , CloseFile input ]

case_join :: Assertion
case_join = do
  let input1 = "README.md"
      input2 = "LICENSE"
  (r, acts) <- runPipeM $
        (reader input1 >> reader input2)
    >+> writer tmpOutput
  assertRight r $ \_ -> return ()

  acts @?=
    [ OpenFile input1 ReadMode
    , OpenFile tmpOutput WriteMode
    , CloseFile input1
    , OpenFile input2 ReadMode
    , CloseFile input2
    , CloseFile tmpOutput ]

  content1 <- readFile input1
  content2 <- readFile input2
  content3 <- readFile tmpOutput
  content3 @?= content1 ++ content2

case_recover :: Assertion
case_recover = do
  let
    input1 = "README.md"
    input2 = "/nonexistent/file"
    safeReader fp = catch (reader fp) $ \e ->
      exec $ saveAction (CaughtException e)

    isException (CaughtException e) = isNonexistingException (E.toException e)
    isException x = assertFailure $ "expected exception, got " ++ show x

  (r, acts) <- runPipeM $
        (safeReader input1 >> safeReader input2)
    >+> writer tmpOutput
  assertRight r $ \_ -> return ()

  zipWithM_ (flip ($)) acts
    [ (@?= OpenFile input1 ReadMode)
    , (@?= OpenFile tmpOutput WriteMode)
    , (@?= CloseFile input1)
    , (@?= OpenFile input2 ReadMode)
    , isException
    , (@?= CloseFile tmpOutput) ]

  equalFiles input1 tmpOutput

main :: IO ()
main = $(defaultMainGenerator)
