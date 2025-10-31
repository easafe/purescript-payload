module Payload.Client.EncodeBody where

import Data.Maybe (Maybe(..))
import Payload.ContentType (class HasContentType)
import Data.Argonaut.Encode.Class
import Prelude
import Data.Argonaut

class (HasContentType body) <= EncodeBody body where
  encodeBody :: body -> String

instance encodeBodyString :: EncodeBody String where
  encodeBody b = b

instance encodeBodyRecord :: EncodeJson (Record r) => EncodeBody (Record r) where
  encodeBody = stringify <<< encodeJson

instance encodeBodyArray :: EncodeJson (Array r) => EncodeBody (Array r) where
  encodeBody = stringify <<< encodeJson

instance encodeBodyMaybe :: EncodeBody a => EncodeBody (Maybe a) where
  encodeBody Nothing = ""
  encodeBody (Just body) = encodeBody body
