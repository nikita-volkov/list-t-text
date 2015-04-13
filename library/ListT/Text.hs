module ListT.Text where

import BasePrelude hiding (cons, uncons)
import MTLPrelude
import ListT
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString as B


-- |
-- A transformation of a stream of byte-strings 
-- to a stream of text chunks decoded using UTF-8.
utf8ByteString :: Monad m => Transformation m ByteString Text
utf8ByteString =
  loop TE.streamDecodeUtf8
  where
    loop decode stream =
      lift (uncons stream) >>= \case
        Nothing -> mzero
        Just (chunk, stream') -> 
          decode chunk & \(TE.Some result leftover decode') ->
            bool (cons result) id (T.null result) (loop decode' stream')


