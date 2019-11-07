module Crapi.Media where

import Prelude
import Control.Comonad (extract)
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textPlain)
import Foreign (renderForeignError)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Type.Proxy (Proxy)

class IsMedia a where
  decodeMedia :: String -> Either String a
  encodeMedia :: a -> String
  mediaType :: Proxy a -> MediaType

instance isMediaUnit :: IsMedia Unit where
  decodeMedia _ = pure unit
  encodeMedia _ = mempty
  mediaType _ = textPlain

instance isMediaString :: IsMedia String where
  decodeMedia = pure
  encodeMedia = identity
  mediaType _ = textPlain

newtype JSON a
  = JSON a

instance isMediaJSON :: (ReadForeign a, WriteForeign a) => IsMedia (JSON a) where
  decodeMedia = readJSON >>> bimap (renderForeignError <<< extract) JSON
  encodeMedia (JSON a) = writeJSON a
  mediaType _ = applicationJSON
