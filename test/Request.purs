module Test.Request 
  ( requestComments
  , requestTune
  , requestTuneAbc
  , requestTuneSearch
  , requestUsers
  , checkUser
  ) 
    where

import Prelude


import Affjax (Request, printError)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.Node (defaultRequest, request)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (fromFoldable)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Routing.Duplex (print)
import TuneBank.Authorization.BasicAuth (authorizationHeader)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre)
import TuneBank.Data.TuneId (TuneId)
import TuneBank.Api.Codec.Comments (Comments, decodeComments)
import TuneBank.Api.Codec.Tune (TuneMetadata, decodeTune)
import TuneBank.Api.Codec.TunesPage (TunesPage, decodeTunesPage)
import TuneBank.Api.Codec.UsersPage (UsersPage, decodeUsersPage)
import TuneBank.Navigation.Endpoint (PageParams, Endpoint(..), endpointCodec)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.Navigation.SearchParams (SearchParams)
import Unsafe.Coerce (unsafeCoerce)

-- | This module mimics that of TuneBank.Api.Request with the proviso that it uses
-- | Affjax in its Node variety instead of its Web variety for obvious reasons
-- | This makes the tests more fragile than they would have been without the disparity
-- | It's important that this is kept in lockstep with TuneBank.Api.Request


requestTune :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String TuneMetadata)
requestTune baseUrl genre tuneId = do
  _ <- liftEffect $ log $ "request tune url: " <> show baseUrl <> print endpointCodec (Tune genre tuneId)
  res <- liftAff $ requestTheBody $ defaultJsonGetRequest baseUrl Nothing (Tune genre tuneId)
  case res of
    Left err ->
      pure $ Left err
    Right json -> do
      let
        -- tune :: Either String Tune
        tune = lmap printJsonDecodeError $ decodeTune json
      pure $ tune


requestTuneAbc :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String String)
requestTuneAbc baseUrl genre tuneId = do
  res <- liftAff $ request $ defaultStringGetRequest baseUrl Nothing (Tune genre tuneId)  (MediaType "text/vnd.abc")
  pure $ bimap printError _.body res


requestTuneSearch :: forall m. MonadAff m => BaseURL -> Genre -> SearchParams -> m (Either String TunesPage)
requestTuneSearch baseUrl genre searchParams = do
  res <- liftAff $ requestTheBody $ defaultJsonGetRequest baseUrl Nothing (Search genre searchParams)
  case res of
    Left err -> do
      pure $ Left err
    Right json -> do
      let
        -- tunesPage :: Either String TunesPage
        tunesPage = lmap printJsonDecodeError $ decodeTunesPage json
      pure $ tunesPage

requestUsers :: forall m. MonadAff m => BaseURL -> Maybe Credentials-> PageParams -> m (Either String UsersPage)
requestUsers baseUrl mCredentials pageParams = do
  res <- liftAff $ requestTheBody $ defaultJsonGetRequest baseUrl mCredentials (Users pageParams)
  case res of
    Left err ->
      pure $ Left err
    Right json -> do
      let
        -- usersPage :: Either String UsersPage
        usersPage = lmap printJsonDecodeError $ decodeUsersPage json
      pure $ usersPage


checkUser :: forall m. MonadAff m => BaseURL -> Credentials-> m (Either String String)
checkUser baseUrl credentials = do
  liftAff $ requestTheBody $ defaultStringGetRequest baseUrl (Just credentials) UserCheck (MediaType "text/plain; charset=UTF-8")
  

requestComments :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String Comments)
requestComments baseUrl genre tuneId = do
  res <- liftAff $ requestTheBody $ defaultJsonGetRequest baseUrl Nothing (Comments genre tuneId)
  case res of
    Left err ->
      pure $ Left err
    Right json -> do
      let
        comments :: Either String Comments
        comments = lmap printJsonDecodeError $ decodeComments json
      pure $ comments

defaultJsonGetRequest :: BaseURL -> Maybe Credentials -> Endpoint -> Request Json
defaultJsonGetRequest (BaseURL baseUrl) mCredentials endpoint =
  let
    url = baseUrl <> print endpointCodec endpoint

    headers = [  Accept (MediaType "application/json; charset=UTF-8")
              ] <> (fromFoldable $ authorizationHeader mCredentials)
    responseFormat = RF.json 
  in
    defaultRequest { url = url, headers = headers, responseFormat = responseFormat }


defaultStringGetRequest :: BaseURL -> Maybe Credentials -> Endpoint -> MediaType -> Request String
defaultStringGetRequest (BaseURL baseUrl) mCredentials endpoint mediaType =
  let 
    url = baseUrl <> print endpointCodec endpoint
    headers = [ Accept mediaType] <>
               (fromFoldable $ authorizationHeader mCredentials)
    responseFormat = RF.string
  in 
    defaultRequest { url = url, headers = headers, responseFormat = responseFormat }



-- | The default manner of attempting a request. We're only interested in the
-- | response body and all errors will be converted to strings
requestTheBody :: ∀ a m. MonadAff m => Request a -> m (Either String a)
requestTheBody r = liftAff do
  response <- request r 
  case response of 
    -- Aff error (unlikely)
    Left err ->
      pure $ Left $ printError err 
    Right result ->
      let 
        -- get the HTTP status range - e.g. 2xx is represented as 2
        statusRange :: Int
        statusRange = (unwrap result.status) / 100
      in
      -- HTTP error ranges
      case statusRange of 
        -- bad request 4xx.  likely in uploading ABC tunes which induce validation on the server
        4 ->
          pure $ Left $ unsafeCoerce result.body
        -- success 2xx.
        2 ->
          pure $ Right result.body
        -- other stuff - chaos or less likely
        _ ->
          pure $ Left $ result.statusText

