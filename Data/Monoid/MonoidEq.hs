module Data.Monoid.MonoidEq
  ( MonoidEq(..)
  ) where

import Data.Monoid

import qualified Data.ByteString as B
import qualified Data.Text as T

import Prelude hiding (null)
import qualified Prelude

class (Eq m, Monoid m) => MonoidEq m where
  null :: m -> Bool
  null = (== mempty)

instance Eq a => MonoidEq [a] where
  null = Prelude.null

instance MonoidEq B.ByteString where
  null = B.null

instance MonoidEq T.Text where
  null = T.null
