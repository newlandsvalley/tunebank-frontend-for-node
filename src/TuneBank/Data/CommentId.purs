module TuneBank.Data.CommentId
  ( CommentId(..)
  , commentIdFromString
  , commentIdToString
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

commentIdToString :: CommentId -> String
commentIdToString (CommentId s) =
  show s


commentIdFromString :: String -> Either String CommentId
commentIdFromString s =
  Right $ CommentId $ fromMaybe 0 (fromString s)


