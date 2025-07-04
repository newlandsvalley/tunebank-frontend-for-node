module TuneBank.Page.Comment where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (length)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import TuneBank.Api.Codec.Comments (NewComment, cleanComment, defaultComment)
import TuneBank.Api.Codec.Utils (showJsonErrorResponse)
import TuneBank.Api.Request (postNewComment, postUpdatedComment, requestComment)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre)
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (TuneId(..))
import TuneBank.Data.CommentId (CommentId)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getUser)

-- | Edit a comment
-- | This is used both for editing existing comments and new ones

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , currentUser :: Maybe Credentials
  , tuneId :: TuneId
  , baseURL :: BaseURL
  , commentId :: Maybe CommentId
  , submission :: NewComment
  , submitCommentResult :: Either String String -- result from server
  }

type Query :: forall k. k -> Type
type Query = (Const Void)

type Input =
  { genre :: Genre
  , tuneId :: TuneId
  , commentId :: Maybe CommentId
  }

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = Initialize
  | HandleSubject String
  | HandleText String
  | SubmitComment MouseEvent

component
  :: ∀ o m r
   . MonadAff m
  => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
  => Navigate m
  => H.Component Query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Nothing
        }
    }
  where

  initialState :: Input -> State
  initialState input =
    { genre: input.genre
    , currentUser: Nothing
    , tuneId: input.tuneId
    , baseURL: BaseURL ""
    , commentId: input.commentId
    , submission: defaultComment
    , submitCommentResult: Right ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    if (isNothing state.currentUser) then
      HH.div_
        [ HH.form
            [ HP.id "commentform" ]
            [ HH.text "you must log in before submitting comments" ]
        ]
    else
      renderForm state

  renderForm :: State -> H.ComponentHTML Action ChildSlots m
  renderForm state =
    let
      (TuneId title) = state.tuneId
    in
      HH.div_
        [ HH.h2
            [ css "center" ]
            [ HH.text ("comment for " <> title) ]
        , HH.form
            [ HP.id "commentform" ]
            [ HH.fieldset
                []
                [ HH.legend_ [ HH.text "Comment" ]
                , renderSubject state
                , renderText state
                , renderAdvisoryText state
                , renderSubmitButton state
                ]
            , renderSubmissionError state
            ]
        ]

  renderAdvisoryText :: State -> H.ComponentHTML Action ChildSlots m
  renderAdvisoryText _state =
    let
      text1 =
        "Your comments can simply be text which may contain links to other sites"
          <> " with recordings or other information about the tune. If you find a YouTube "
          <>
            " video of the tune, the best technique is to follow "

      text2 =
        " to embed the video and then copy the code they provide into the text box."
          <> " Alternatively you can copy the watch URL of the video you're viewing from"
          <>
            " your web browser’s address bar and we'll attempt to embed the video into the comment"
    in
      HH.div_
        [ HH.p
            []
            [ HH.text text1
            , HH.a
                [ HP.href "https://support.google.com/youtube/answer/171780?hl=en-GB" ]
                [ HH.text "these instructions" ]
            , HH.text text2
            ]
        ]

  renderSubject :: State -> H.ComponentHTML Action ChildSlots m
  renderSubject state =
    HH.div
      [ css "textinput-div" ]
      [ HH.label
          [ css "textinput-label" ]
          [ HH.text "subject:" ]
      , HH.input
          [ css "comment-textinput-subject"
          , HE.onValueInput HandleSubject
          , HP.value state.submission.subject
          , HP.type_ HP.InputText
          ]
      ]

  -- we allow any text but embedded double quotes are problematic
  renderText :: State -> H.ComponentHTML Action ChildSlots m
  renderText state =
    HH.div
      [ HP.id "comment-textinput-div" ]
      [ HH.label
          [ HP.id "comment-textinput-label" ]
          [ HH.text "text:" ]
      , HH.textarea
          [ HP.autofocus true
          , HP.value state.submission.text
          , css "comment-edit"
          , HE.onValueInput HandleText
          ]
      ]

  renderSubmitButton :: State -> H.ComponentHTML Action ChildSlots m
  renderSubmitButton state =
    let
      enabled =
        (length state.submission.subject > 0) &&
          (length state.submission.text > 0)
      className =
        if enabled then "hoverable" else "unhoverable"
    in
      HH.button
        [ HE.onClick SubmitComment
        , css className
        , HP.enabled enabled
        ]
        [ HH.text "submit comment" ]

  renderSubmissionError :: State -> H.ComponentHTML Action ChildSlots m
  renderSubmissionError state =
    let
      errorText = either (\c -> "comment update failed: " <> showJsonErrorResponse c) (const "") state.submitCommentResult
    in
      HH.div_
        [ HH.text errorText ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      currentUser <- getUser
      baseURL <- getBaseURL
      let
        newState = state
          { currentUser = currentUser
          , baseURL = baseURL
          }
      case newState.currentUser of
        Just credentials -> do
          case newState.commentId of
            Nothing -> do
              H.put newState
              pure unit
            Just commentId -> do
              eComment <- H.liftAff $ requestComment baseURL commentId credentials
              case eComment of
                Right comment -> do
                  -- we have a full comment back from the database, allowing us to edit the subject or text
                  -- edit comment
                  let
                    submission = { subject: comment.subject, text: comment.text }
                  H.modify_ (\st -> st { baseURL = baseURL, currentUser = currentUser, submission = submission })
                  pure unit
                Left _ -> do
                  -- new comment
                  H.put newState
                  pure unit
        Nothing ->
          -- a user login is required before anything can happen in this page
          pure unit
    HandleSubject subject -> do
      state <- H.get
      let
        newSubmission = state.submission { subject = subject }
      H.modify_ (\st -> st { submission = newSubmission })
    HandleText text -> do
      state <- H.get
      let
        newSubmission = state.submission { text = text }
      H.modify_ (\st -> st { submission = newSubmission })
    SubmitComment event -> do
      _ <- H.liftEffect $ preventDefault $ toEvent event
      state <- H.get
      case state.currentUser of
        Nothing ->
          pure unit
        Just credentials -> do
          baseURL <- getBaseURL
          let
            -- clean the comment of any characters which may ruin the eventual JSON - particularly double quotes
            submission = cleanComment state.submission

          submitCommentResult <- case state.commentId of
            Nothing ->
              H.liftAff $ postNewComment baseURL state.genre state.tuneId submission credentials
            Just commentId ->
              H.liftAff $ postUpdatedComment baseURL commentId submission credentials
          H.modify_ (\st -> st { submitCommentResult = submitCommentResult })
          case submitCommentResult of
            Left _ ->
              -- stay on this page, the error should now be rendered
              pure unit
            Right _ ->
              -- go back to the tune page which should now show the comment
              navigate $ Tune state.genre state.tuneId

