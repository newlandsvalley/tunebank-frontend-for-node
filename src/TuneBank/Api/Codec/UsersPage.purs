module TuneBank.Api.Codec.UsersPage
  ( UsersPage
  , UserRefArray
  , UserRef
  , decodeUsersPage
  ) where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (Either)
import Data.Traversable (traverse)
import TuneBank.Api.Codec.Pagination (Pagination, decodeJsonPagination)

type UserRef =
  { name :: String
  , email :: String
  , role :: String
  , valid :: String
  , timestamp :: Int
  }

decodeJsonUserRef :: Json -> Either JsonDecodeError UserRef
decodeJsonUserRef json = do
  obj <- decodeJson json
  name <- obj .: "username"
  email <- obj .: "email"
  role <- obj .: "role"
  valid <- obj .: "valid"
  timestamp <- obj .: "timestamp"
  pure $ { name, email, role, valid, timestamp }

type UserRefArray = Array UserRef

type UsersPage =
  { users :: UserRefArray
  , pagination :: Pagination
  }

decodeUserRefArray :: Json -> Either JsonDecodeError UserRefArray
decodeUserRefArray json = decodeJson json >>= traverse decodeJsonUserRef

decodeUsersPage :: Json -> Either JsonDecodeError UsersPage
decodeUsersPage json = do
  obj <- decodeJson json
  users <- obj .: "users" >>= decodeUserRefArray
  pagination <- obj .: "pagination" >>= decodeJsonPagination
  pure $ { users, pagination }
