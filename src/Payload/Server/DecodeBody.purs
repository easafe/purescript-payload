module Payload.Server.DecodeBody
       ( class DecodeBody
       , decodeBody
       ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Argonaut.Parser
import Data.Argonaut.Decode
import Data.Maybe (Maybe(..))

class DecodeBody body where
  decodeBody :: String -> Either String body

instance decodeBodyRecord :: DecodeJson (Record r) => DecodeBody (Record r) where
  decodeBody body = jsonParser body >>= (lmap show <<< decodeJson)

instance decodeBodyArray :: DecodeJson (Array r) => DecodeBody (Array r) where
  decodeBody body  = jsonParser body >>= (lmap show <<< decodeJson)

instance decodeBodyString :: DecodeBody String where
  decodeBody = pure

instance decodeBodyMaybe :: DecodeBody a => DecodeBody (Maybe a) where
  decodeBody "" = pure Nothing
  decodeBody str = Just <$> decodeBody str
