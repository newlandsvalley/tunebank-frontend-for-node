module TuneBank.Data.Credentials
  ( Credentials
  , Role(..)
  , blankCredentials
  , roleFromString
  ) where

import Prelude (class Eq, class Ord, class Show, (==))

-- | the user role
data Role
  = NormalUser
  | Administrator

derive instance eqRole :: Eq Role
derive instance ordRole :: Ord Role

instance showRole :: Show Role where
  show NormalUser = "normal user"
  show Administrator = "administrator"

roleFromString :: String -> Role
roleFromString s =
  if (s == "administrator") then
    Administrator
  else
    NormalUser

-- | the credentials of a user
type Credentials =
  { user :: String
  , pass :: String
  , role :: Role
  }

-- | blanked out (uninitialised) credentials
blankCredentials :: Credentials
blankCredentials =
  { user: ""
  , pass: ""
  , role: NormalUser
  }

