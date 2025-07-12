module TuneBank.Page.Utils.UserValidation
  ( validatePassword
  ) where

-- | routine to validate a user password
-- | used both by Register and ChangePassword

import Prelude

import Data.String (length)
import Data.Validation.Semigroup (invalid)
import TuneBank.Data.Types (Validated)
import TuneBank.Api.Codec.Utils (containsDigit)

validatePassword :: String -> String -> Validated String
validatePassword password confirmationPassword =
  if (containsDigit password) && length password >= 7 then
    comparePasswords password confirmationPassword
  else
    invalid $ pure ("Passwords should be at least 7 characters and contain at least one digit. ")

comparePasswords :: String -> String -> Validated String
comparePasswords password confirmationPassword =
  if (password /= confirmationPassword) then
    invalid $ pure ("Passwords don't match. ")
  else
    pure password
