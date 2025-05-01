module TuneBank.Data.CommentId
  ( CommentId(..)
  , CommentKey
  , commentIdFromString
  , commentIdToString
  , commentKey
  ) where

import Prelude
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Either (Either(..))

newtype CommentId = CommentId Int

derive newtype instance eqCommentId :: Eq CommentId
derive newtype instance ordCommentId :: Ord CommentId


instance showCommentid :: Show CommentId where
  show = commentIdToString

-- the key to a comment is the combinatio  of the user who originally submitted the
-- | comment and the commentId (a timestamp)
type CommentKey =
  { user :: String
  , commentId :: CommentId
  }

commentIdToString :: CommentId -> String
commentIdToString (CommentId s) =
  show s


-- TO DO
commentIdFromString :: String -> Either String CommentId
commentIdFromString s =
  Right $ CommentId $ fromMaybe 0 (fromString s)

commentKey :: String -> CommentId -> CommentKey
commentKey user commentId =
  { user, commentId }
