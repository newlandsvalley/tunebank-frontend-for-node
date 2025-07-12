module TuneBank.Api.Codec.Register
  ( Submission
  , defaultSubmission
  , encodeSubmission
  ) where

import Data.Argonaut (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import TuneBank.Navigation.Route (Route(..), routeCodec)
import Routing.Duplex (print)

-- the form data to be posted in a new user registration
type Submission =
  { name :: String
  , email :: String
  , password :: String
  , password2 :: String
  , refererUrl :: String
  }

encodeSubmission :: Submission -> Json
encodeSubmission =
  encodeJson

defaultSubmission :: Submission
defaultSubmission =
  { name: ""
  , email: ""
  , password: ""
  , password2: ""
  , refererUrl: print routeCodec Register
  }

