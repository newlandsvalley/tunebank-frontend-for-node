module TuneBank.Data.Uuid
  ( Uuid(..)
  , uuidFromString
  , uuidToString
  , decodeUuidURIComponent
  , encodeUuidURIComponent
  ) where

-- | The Uuid is a uniqe OTP code emailed to a user after she starts the registration process
-- | which she must return via a link in order to complete registration

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import TuneBank.Api.Codec.Utils (unsafeEncodeURIComponent)

newtype Uuid =  Uuid String

derive instance genericUuid :: Generic Uuid _
derive instance eqUuid :: Eq Uuid
derive instance ordUuid :: Ord Uuid

instance showUuid :: Show Uuid where
  show = uuidToString

uuidToString :: Uuid -> String
uuidToString (Uuid s) =
  s

uuidFromString :: String -> Either String Uuid
uuidFromString s =
  Right $ Uuid s

-- | decode a UUid coming back from the tunebank-node server
decodeUuidURIComponent :: String -> Uuid
decodeUuidURIComponent s =
  Uuid s

encodeUuidURIComponent :: Uuid -> String
encodeUuidURIComponent =
  unsafeEncodeURIComponent <<< uuidToString

