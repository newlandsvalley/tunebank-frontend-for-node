module TuneBank.Data.TuneId
  ( TuneId(..)
  , tuneIdFromString
  , tuneIdToString
  , decodeTuneIdURIComponent
  , encodeTuneIdURIComponent
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import TuneBank.Api.Codec.Utils (unsafeEncodeURIComponent)

newtype TuneId = TuneId String 

derive instance genericTuneId :: Generic TuneId _
derive instance eqTuneId :: Eq TuneId
derive instance ordTuneId :: Ord TuneId

instance showTuneid :: Show TuneId where
  show = tuneIdToString

tuneIdToString :: TuneId -> String
tuneIdToString (TuneId title ) =
  title

tuneIdFromString :: String -> Either String TuneId
tuneIdFromString s =
  Right $ TuneId s

-- | decode a tuneId coming back from the tunebank-node server
decodeTuneIdURIComponent :: String -> TuneId
decodeTuneIdURIComponent s =
  TuneId s
  -- tuneIdFromString

encodeTuneIdURIComponent :: TuneId -> String
encodeTuneIdURIComponent =
  unsafeEncodeURIComponent <<< tuneIdToString

