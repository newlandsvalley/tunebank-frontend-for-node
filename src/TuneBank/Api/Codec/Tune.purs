module TuneBank.Api.Codec.Tune
  ( TuneMetadata(..)
  , nullTuneMetadata
  , decodeTune
  ) where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (Either)
import Data.Maybe (Maybe(..))

type TuneMetadata =
  { title :: String
  , source :: Maybe String
  , composer :: Maybe String
  , origin :: Maybe String
  , transcriber :: Maybe String
  , submitter :: String
  , tid :: Int
  , ts :: Int
  , abc :: String
  }

nullTuneMetadata :: TuneMetadata
nullTuneMetadata =
  { title: ""
  , source: Nothing
  , composer: Nothing
  , origin: Nothing
  , transcriber: Nothing
  , submitter: ""
  , tid: 0
  , ts: 0
  , abc: ""
  }

decodeTune :: Json -> Either JsonDecodeError TuneMetadata
decodeTune json = do
  obj <- decodeJson json
  title <- obj .: "title"
  source <- obj .:? "source"
  composer <- obj .:? "composer"
  origin <- obj .:? "origin"
  transcriber <- obj .:? "transcriber"
  submitter <- obj .: "submitter"
  tid <- obj .: "id"
  abc <- obj .: "abc"
  ts <- obj .: "timestamp"
  pure $ { title, source, composer, origin, transcriber, submitter, tid, ts, abc }
