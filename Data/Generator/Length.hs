{-# LANGUAGE TypeFamilies #-}
module Data.Generator.Length where

import qualified Data.ByteString as B
import qualified Data.Foldable as F
import Data.Generator
import Data.Semigroup.Reducer
import qualified Data.Text as T
import Prelude hiding (length)
import qualified Prelude

class Generator g => GeneratorLength g where
  length :: g -> Int
  length = getCount . reduce

instance GeneratorLength B.ByteString where
  length = B.length

instance GeneratorLength T.Text where
  length = T.length
