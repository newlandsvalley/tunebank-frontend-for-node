module TuneBank.Api.Codec.Password
  ( OTPSubmission
  , ChangePassword
  , encodeOTPSubmission
  , defaultOTPSubmission
  , encodeChangePassword
  , defaultChangePassword
  ) where

import Data.Argonaut (Json)
import Data.Argonaut.Encode.Class (encodeJson)

-- | the form data to be posted with the One-Time-Password to authorise an later change password
type OTPSubmission =
  { name :: String
  , otp :: String
  }

encodeOTPSubmission :: OTPSubmission -> Json
encodeOTPSubmission =
  encodeJson

defaultOTPSubmission :: OTPSubmission
defaultOTPSubmission =
  { name: ""
  , otp: ""
  }

-- | the form posted for the actual password change request
type ChangePassword =
  { name :: String
  , password :: String
  }

encodeChangePassword :: ChangePassword -> Json
encodeChangePassword =
  encodeJson

defaultChangePassword :: ChangePassword
defaultChangePassword =
  { name: ""
  , password: ""
  }