module Payload.Server.QueryParams
       ( class DecodeQueryParam
       , decodeQueryParam
       , class DecodeQueryParamMulti
       , decodeQueryParamMulti
       , DecodeError(QueryDecodeError, QueryParamNotFound)
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.DateTime.Instant as DDI
import Data.Time.Duration as DTD
import Foreign.Object (Object)
import Data.Number as DNM
import Data.DateTime(DateTime)
import Foreign.Object as Object
import Payload.Server.Internal.Querystring (ParsedQuery)

data DecodeError
  = QueryDecodeError {key :: String, values :: Array String, message :: String, queryObj :: ParsedQuery}
  | QueryParamNotFound {key :: String, queryObj :: ParsedQuery}

instance showDecodeError :: Show DecodeError where
  show (QueryDecodeError e) = "(QueryDecodeError " <> show e <> ")"
  show (QueryParamNotFound e) = "(QueryParamNotFound " <> show e <> ")"

class DecodeQueryParam a where
  decodeQueryParam :: ParsedQuery -> String -> Either DecodeError a

instance decodeQueryParamInt :: DecodeQueryParam Int where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Left (QueryParamNotFound {key: queryKey, queryObj})
      Just [] -> decodeErr [] $ "Expected single value but received empty array."
      Just [str] -> maybe (decodeErr [str] "Could not decode into an Int") Right (Int.fromString str)
      Just arr -> decodeErr arr $ "Expected single value but received multiple: " <> show arr
    where
      decodeErr values msg = Left (QueryDecodeError {key: queryKey, values, message: msg, queryObj})

instance decodeQueryParamString :: DecodeQueryParam String where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Left (QueryParamNotFound {key: queryKey, queryObj})
      Just [] -> decodeErr [] $ "Expected single value but received empty Array"
      Just [str] -> Right str
      Just arr -> decodeErr arr $ "Expected single value but received multiple: " <> show arr
    where
      decodeErr values msg = Left (QueryDecodeError {key: queryKey, values, message: msg, queryObj})

instance decodeQueryParamBoolean :: DecodeQueryParam Boolean where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Left (QueryParamNotFound {key: queryKey, queryObj})
      Just [] -> decodeErr [] $ "Expected single value but received empty Array"
      Just ["false"] -> Right false
      Just ["true"] -> Right true
      Just arr -> decodeErr arr $ "Expected single value but received multiple: " <> show arr
    where
      decodeErr values msg = Left (QueryDecodeError {key: queryKey, values, message: msg, queryObj})

instance decodeQueryParamMaybe :: DecodeQueryParam a => DecodeQueryParam (Maybe a) where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Right Nothing
      Just [] -> Right Nothing
      Just [""] -> Right Nothing
      Just _ -> Just <$> decodeQueryParam queryObj queryKey

instance DecodeQueryParam DateTime where
      decodeQueryParam query key =
            case Object.lookup key query of
                  Nothing → Left $ QueryParamNotFound { key, queryObj: query }
                  Just [ value ] → maybe (errorDecoding query key) (Right <<< DDI.toDateTime) (DDI.instant <<< DTD.Milliseconds =<< DNM.fromString value)
                  _ → errorDecoding query key

errorDecoding ∷ ∀ a. Object (Array String) → String → Either DecodeError a
errorDecoding queryObj key = Left $ QueryDecodeError
      { values: []
      , message: "Could not decode parameter " <> key
      , key
      , queryObj
      }

class DecodeQueryParamMulti a where
  decodeQueryParamMulti :: ParsedQuery -> Either DecodeError a

instance decodeQueryParamMultiObjectString :: DecodeQueryParamMulti (Object (Array String)) where
  decodeQueryParamMulti o = Right o
