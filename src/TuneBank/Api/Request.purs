module TuneBank.Api.Request 
  ( checkUser
  , checkService
  , deleteComment
  , deleteTune
  , deleteUser
  , postNewComment
  , postNewUser  
  , postTune
  , postUpdatedComment
  , requestComment
  , requestComments
  , requestTune
  , requestTuneAbc
  , requestTuneSearch
  , requestUsers
  ) where

import Prelude

import Affjax (Request, printError)
import Affjax.RequestBody (formURLEncoded, json, string)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (defaultRequest, request)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (fromFoldable)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.FormURLEncoded (FormURLEncoded) as FUE
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
-- import Data.MediaType.Common (applicationJSON)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Class (liftEffect)
import Halogen as H
import Routing.Duplex (print)
import TuneBank.Api.Codec.Comments (Comment, Comments, decodeComment, decodeComments)
import TuneBank.Api.Codec.Comments (NewComment, encodeNewComment) as Comments
import TuneBank.Api.Codec.Register (Submission, encodeSubmission) as Register
import TuneBank.Api.Codec.Tune (TuneMetadata, decodeTune)
import TuneBank.Api.Codec.TunesPage (TunesPage, decodeTunesPage)
import TuneBank.Api.Codec.UsersPage (UsersPage, decodeUsersPage)
import TuneBank.Authorization.BasicAuth (authorizationHeader)
import TuneBank.Data.CommentId (CommentId)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre)
import TuneBank.Data.TuneId (TuneId)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.Data.UserId (UserId)
import TuneBank.Navigation.Endpoint (PageParams, Endpoint(..), endpointCodec)
import TuneBank.Navigation.SearchParams (SearchParams)
import Unsafe.Coerce (unsafeCoerce)

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

{-}
-- | only needed against pre v1.2.0 musicrest which exhibits JSON errors
defaultJsonAsStrGetRequest :: BaseURL -> Maybe Credentials -> Endpoint -> Request String
defaultJsonAsStrGetRequest (BaseURL baseUrl) mCredentials endpoint =
  let
    url = baseUrl <> print endpointCodec endpoint
    headers = [Accept applicationJSON ]<>
                (fromFoldable $ authorizationHeader mCredentials)
    responseFormat = RF.string
  in 
    defaultRequest { url = url, headers = headers, responseFormat = responseFormat }
-}
   

{-}
-- a default POST request for form URL-encoded data
defaultPostRequest :: BaseURL -> Maybe Credentials -> FUE.FormURLEncoded -> Endpoint -> Request String
defaultPostRequest (BaseURL baseUrl) mCredentials fue endpoint  =
  let 
    method = Left POST
    url = baseUrl <> print endpointCodec endpoint
    headers = fromFoldable $ authorizationHeader mCredentials
    content = Just $ formURLEncoded fue
    responseFormat = RF.string
  in  
    defaultRequest { method = method
                   , url = url
                   , headers = headers
                   , content = content
                   , responseFormat = responseFormat }
-}


-- A default POST request for JSON bodies
defaultPostJsonRequest :: BaseURL -> Maybe Credentials -> Json -> Endpoint -> Request String
defaultPostJsonRequest (BaseURL baseUrl) mCredentials body endpoint  =
  let 
    method = Left POST
    url = baseUrl <> print endpointCodec endpoint
    headers = fromFoldable $ authorizationHeader mCredentials
    content = Just $ json body
    responseFormat = RF.string
  in  
    defaultRequest { method = method
                   , url = url
                   , headers = headers
                   , content = content
                   , responseFormat = responseFormat }


-- a default POST request for simple string bodies
defaultPostStringRequest :: BaseURL -> Maybe Credentials -> String -> Endpoint -> Request String
defaultPostStringRequest (BaseURL baseUrl) mCredentials s endpoint  =
  let 
    method = Left POST
    url = baseUrl <> print endpointCodec endpoint
    headers = fromFoldable $ authorizationHeader mCredentials
    content = Just $ string s
    responseFormat = RF.string
  in  
    defaultRequest { method = method
                   , url = url
                   , headers = headers
                   , content = content
                   , responseFormat = responseFormat }




defaultDeleteRequest :: BaseURL -> Maybe Credentials -> Endpoint -> Request String
defaultDeleteRequest (BaseURL baseUrl) mCredentials endpoint  =
  let 
    method = Left DELETE
    url = baseUrl <> print endpointCodec endpoint
    -- _foo = spy "delete URL" url
    headers = fromFoldable $ authorizationHeader mCredentials
    responseFormat = RF.string
  in  
    defaultRequest { method = method
                   , url = url
                   , headers = headers
                   , responseFormat = responseFormat }

-- | this gives a bad JSON error because it really is bad!
requestTune :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String TuneMetadata)
requestTune baseUrl genre tuneId = do
  res <- H.liftAff $ requestTheBody $ defaultJsonGetRequest baseUrl Nothing (Tune genre tuneId)
  case res of
    Left err ->
      pure $ Left err
    Right json -> do
      let
        -- tune :: Either String Tune
        tune = lmap printJsonDecodeError $ decodeTune json
      pure $ tune


{-}
requestTuneStr :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String String)
requestTuneStr baseUrl genre tuneId = do
  res <- H.liftAff $ requestTheBody $ defaultJsonAsStrGetRequest baseUrl Nothing (Tune genre tuneId)
  pure res
-}

requestTuneAbc :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String String)
requestTuneAbc baseUrl genre tuneId = do
  res <- H.liftAff $ request $ defaultStringGetRequest baseUrl Nothing (Tune genre tuneId)  (MediaType "text/vnd.abc")
  pure $ bimap printError _.body res

requestTuneSearch :: forall m. MonadAff m => BaseURL -> String -> SearchParams -> m (Either String TunesPage)
requestTuneSearch baseUrl genre searchParams = do
  res <- H.liftAff $ requestTheBody $ defaultJsonGetRequest baseUrl Nothing (Search genre searchParams)
  case res of
    Left err -> do
      _ <- liftEffect $ log ("we got an Affjax error: " <> err)
      pure $ Left err
    Right json -> do
      let
        -- tunesPage :: Either String TunesPage
        tunesPage = lmap printJsonDecodeError $ decodeTunesPage json
      _ <- liftEffect $ log ("we got some JSON: ")
      pure $ tunesPage


requestUsers :: forall m. MonadAff m => BaseURL -> Maybe Credentials-> PageParams -> m (Either String UsersPage)
requestUsers baseUrl mCredentials pageParams = do
  res <- H.liftAff $ requestTheBody $ defaultJsonGetRequest baseUrl mCredentials (Users pageParams)
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
  res <- H.liftAff $ requestTheBody $ defaultStringGetRequest baseUrl (Just credentials) UserCheck (MediaType "text/plain; charset=UTF-8")
  pure res 

-- | check the MusicRest service is up by attempting to get a welcome message
-- | we never need to bother to decode the JSON
checkService :: forall m. MonadAff m => BaseURL -> m (Either String Json)
checkService baseUrl =  H.liftAff do
  res <- requestTheBody $ defaultJsonGetRequest baseUrl Nothing Root
  pure res

requestComments :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String Comments)
requestComments baseUrl genre tuneId = do
  res <- H.liftAff $ requestTheBody $ defaultJsonGetRequest baseUrl Nothing (Comments genre tuneId)
  case res of
    Left err ->
      pure $ Left err
    Right json -> do
      let
        comments :: Either String Comments
        comments = lmap printJsonDecodeError $ decodeComments json
      pure $ comments

requestComment :: forall m. MonadAff m => BaseURL -> CommentId -> Credentials -> m (Either String Comment)
requestComment baseUrl commentId credentials =
  H.liftAff do
    res <- requestTheBody $ defaultJsonGetRequest baseUrl (Just credentials) (Comment commentId)
    case res of
      Left err ->
        pure $ Left err
      Right json -> do
        let
          comment :: Either String Comment
          comment = lmap printJsonDecodeError $ decodeComment json
        pure $ comment

{-}
requestTuneSearchStr :: forall m. MonadAff m => BaseURL -> String -> SearchParams -> m (Either String String)
requestTuneSearchStr baseUrl genre searchParams = do
  res <- H.liftAff $ requestTheBody $ defaultJsonAsStrGetRequest baseUrl Nothing (Search genre searchParams)
  pure $ lmap show res

requestCommentsStr :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String String)
requestCommentsStr baseUrl genre tuneId = do
  res <- H.liftAff $ requestTheBody $ defaultJsonAsStrGetRequest baseUrl Nothing (Comments genre tuneId) 
  pure $ lmap show res
-}

-- | POST

postTune :: forall m. MonadAff m => String -> BaseURL -> Genre -> Credentials -> m (Either String String)
postTune tuneAbc baseUrl genre credentials =
  H.liftAff do
    res <- requestTheBody $ defaultPostStringRequest baseUrl (Just credentials) tuneAbc (NewTune genre)
    pure res

postNewUser :: forall m. MonadAff m => Register.Submission -> BaseURL -> m (Either String String)
postNewUser submission baseUrl =
  H.liftAff do
    let
      json = Register.encodeSubmission submission
    res <- requestTheBody $ defaultPostJsonRequest baseUrl Nothing json Register
    pure res


{-}
postComment :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> Comments.Comment -> Credentials -> m (Either String String)
postComment baseUrl genre tuneId comment credentials =
  H.liftAff do
    let
      formData = Comments.encodeFormData comment
    res <- requestTheBody $ defaultPostRequest baseUrl (Just credentials) formData (Comments genre tuneId)
    pure res
-}
postNewComment :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> Comments.NewComment -> Credentials -> m (Either String String)
postNewComment baseUrl genre tuneId comment credentials =
  H.liftAff do
    let
      json = Comments.encodeNewComment comment
    res <- requestTheBody $ defaultPostJsonRequest baseUrl (Just credentials) json (Comments genre tuneId)
    pure res


postUpdatedComment :: forall m. MonadAff m => BaseURL -> CommentId -> Comments.NewComment -> Credentials -> m (Either String String)
postUpdatedComment baseUrl commentId comment credentials =
  H.liftAff do
    let
      json = Comments.encodeNewComment comment
    res <- requestTheBody $ defaultPostJsonRequest baseUrl (Just credentials) json (Comment commentId)
    pure res


-- | DELETE
deleteTune :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> Credentials -> m (Either String String)
deleteTune baseUrl genre tuneId credentials =
  H.liftAff do
    res <- requestTheBody $ defaultDeleteRequest baseUrl (Just credentials) (Tune genre tuneId)
    pure res


deleteComment :: forall m. MonadAff m => BaseURL -> CommentId -> Credentials -> m (Either String String)
deleteComment baseUrl commentId credentials =
  H.liftAff do
    res <- requestTheBody $ defaultDeleteRequest baseUrl (Just credentials) (Comment commentId)
    pure res

deleteUser :: forall m. MonadAff m => BaseURL -> UserId -> Credentials -> m (Either String String)
deleteUser baseUrl userId credentials =
  H.liftAff do
    res <- requestTheBody $ defaultDeleteRequest baseUrl (Just credentials) (User userId)
    pure res    

-- | The default manner of attempting a request. We're only interested in the
-- | response body and all errors will be converted to strings
requestTheBody :: ∀ a m. MonadAff m => Request a -> m (Either String a)
requestTheBody r = H.liftAff do
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

