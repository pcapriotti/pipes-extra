module Control.Pipe.Text
  ( decode
  , encode
  , lines
  , characters
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import Control.Pipe
import qualified Control.Pipe.Chunked as C
import Control.Pipe.Class

import Prelude hiding (lines)

decodeChunk :: T.OnDecodeError -> ByteString -> ByteString -> (Text, ByteString)
decodeChunk on_err prefix' chunk = (text, rest)
  where
    is_first c = c .&. 0xc0 == 0xc0

    prefixl = B.length prefix'
    split
      | B.null prefix' = 0
      | c .&. 0x80 == 0x00 = 1
      | c .&. 0xe0 == 0xc0 = 2
      | c .&. 0xf0 == 0xe0 = 3
      | c .&. 0xf8 == 0xf0 = 4
      | otherwise = prefixl
      where c = B.head prefix'
    split' = max (split - prefixl) 0

    (prefix'', chunk') = B.splitAt split' chunk
    (chunk'', rest)
      | B.null chunk'
      || B.last chunk' .&. 0x80 == 0
      = (chunk', B.empty)
      | otherwise
      = case B.breakEnd is_first chunk'
        of (c,r) | B.null c -> (c,r)
                 | otherwise -> (B.init c, B.cons (B.last c) r)

    text = T.decodeUtf8With on_err $ prefix' <> prefix'' <> chunk''

-- | Convert a utf-8 encoded byte stream into chunks of text
decode :: MonadStream m => T.OnDecodeError -> m ByteString Text r r
decode on_err = withDefer $ go mempty
  where
    go prefix = await >>= \chunk -> case decodeChunk on_err prefix chunk of
      (text, prefix') -> yield text >> go prefix'

-- | Convert chunked text into a utf-8 encoded byte stream
encode :: MonadStream m => m Text ByteString r r
encode = pipe T.encodeUtf8

-- | Split the chunked input stream into lines, and yield them individually.
lines :: MonadStream m => m Text Text r r
lines = C.splitOn '\n'

-- | Yield individual characters of the chunked input stream.
characters :: MonadStream m => m Text Char r r
characters = C.elems
