module TuneBank.Page.Tune where

import Prelude

import Audio.SoundFont (Instrument)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Abc (AbcTune)
import Data.Abc.Melody (PlayableAbc(..), defaultPlayableAbcProperties)
import Data.Abc.Parser (parse)
import Data.Abc.Tempo (defaultTempo, getAbcTempo, getBpm)
import Data.Abc.Utils (getTitle)
import Data.Array (filter, length)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..), either, isRight)
import Data.Int (fromString, toNumber)
import Data.Link (expandLinks, expandYouTubeWatchLinks)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.PlayerComponent as PC
import Html.Renderer.Halogen as RH
import TuneBank.Api.Codec.Comments (Comments, Comment)
import TuneBank.Api.Codec.Tune (TuneMetadata, nullTuneMetadata)
import TuneBank.Api.Request (requestTune, requestComments, deleteComment, deleteTune)
import TuneBank.Data.CommentId (CommentId)
import TuneBank.Data.Credentials (Credentials, Role(..))
import TuneBank.Data.Genre (Genre)
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (TuneId, tuneIdToString)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.HTML.Utils (css, safeHref, renderKV, showRatio, tsToDateString)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getUser, smallDeviceViewportWidthCutoff)
import Tunebank.HTML.Window (print)
import VexFlow.Score (Renderer, clearCanvas, initialiseCanvas, renderFinalTune) as Score
import VexFlow.Types (Config, defaultConfig)
import Type.Proxy (Proxy(..))
import Web.HTML (window) as HTML
import Web.HTML.Window (innerWidth) as Window

nullParsedTune :: Either String AbcTune
nullParsedTune =
  Left "no tune yet"

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , currentUser :: Maybe Credentials
  , tuneId :: TuneId
  , baseURL :: BaseURL
  , tuneMetadata :: TuneMetadata
  , tuneResult :: Either String AbcTune
  , currentBpm :: Int -- BPM from the tempo slider
  , originalBpm :: Int -- BPM from the Abc
  , tempoNoteLength :: String
  , isPlaying :: Boolean
  , comments :: Comments
  , commentsLoadError :: String
  , debugCommentsJSON :: String
  , instruments :: Array Instrument
  , generateIntro :: Boolean
  , deviceViewportWidth :: Int
  , vexConfig :: Config
  }

type Input =
  { genre :: Genre
  , tuneId :: TuneId
  , instruments :: Array Instrument
  }

type Query :: forall k. k -> Type
type Query = (Const Void)

-- type ChildSlots :: ( Player :: Slot PlayableAbc Unit)
type ChildSlots =
  (player :: (PC.Slot PlayableAbc) Unit)

_player = Proxy :: Proxy "player"

data Action
  = Initialize
  | Finalize
  | RenderScore
  | HandleTuneIsPlaying PC.Message
  | ToggleGenerateIntro
  | HandleTempoInput Int
  | DeleteTune TuneId
  | DeleteComment CommentId
  | PrintScore

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
        , finalize = Just Finalize
        }
    }
  where

  initialState :: Input -> State
  initialState input =
    { genre: input.genre
    , currentUser: Nothing
    , tuneId: input.tuneId
    , baseURL: BaseURL ""
    , tuneMetadata: nullTuneMetadata
    , tuneResult: nullParsedTune
    , currentBpm: defaultTempo.bpm -- 120
    , originalBpm: defaultTempo.bpm -- 120
    , tempoNoteLength: "1/4"
    , isPlaying: false
    , comments: []
    , commentsLoadError: ""
    , debugCommentsJSON: ""
    , instruments: input.instruments
    , generateIntro: false
    , deviceViewportWidth: 0
    , vexConfig: vexConfig
    }

    where
    vexConfig :: Config
    vexConfig =
      defaultConfig
        { parentElementId = "vexflow"
        , width = 1400
        , height = 200
        , scale = 0.8
        , isSVG = true
        }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ renderScore
      , HH.div
          [ HP.id "tune-metadata" ]
          [ renderTuneMetadata state
          , renderTempoSlider state
          , renderPlayer state
          , renderIntroButton state
          , renderComments state
          , renderParseError state
          -- , renderDebugViewportWidth state
          ]
      ]

  renderScore :: H.ComponentHTML Action ChildSlots m
  renderScore =
    HH.div
      [ HP.id "score"
      , HP.class_ (H.ClassName "center")
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "canvasDiv")
          , HP.id "vexflow"
          ]
          []
      ]

  {- get the tune image from the server instead
  renderTuneScore :: State -> String -> H.ComponentHTML Action ChildSlots m
  renderTuneScore state title =
    let
      imageURI = urlPreface state <> "/png"
    in
      HH.div
        [ HP.id_ "score"
        , css "center-image"
        ]
        [HH.img
          [ HP.src imageURI
          , HP.alt title
          ]
        ]
  -}

  renderTuneMetadata :: State -> H.ComponentHTML Action ChildSlots m
  renderTuneMetadata state =
    HH.dl
      []
      [ renderKV "submitter" state.tuneMetadata.submitter
      , renderOptionalKV "composer" state.tuneMetadata.composer
      , renderOptionalKV "source" state.tuneMetadata.source
      , renderOptionalKV "origin" state.tuneMetadata.origin
      , renderOptionalKV "transcriber" state.tuneMetadata.transcriber
      , HH.dt
          []
          [ HH.text "download" ]
      , HH.dd
          []
          [ HH.a
              [ HP.href (urlPreface state <> "/abc")
              , HP.type_ (MediaType "text/vnd.abc")
              ]
              [ HH.text "abc" ]

          , HH.a
              [ HP.href (urlPreface state <> "/midi")
              , HP.type_ (MediaType "audio/midi")
              ]
              [ HH.text "midi" ]
          ]
      , renderTuneControls state
      ]

  renderOptionalKV :: String -> Maybe String -> H.ComponentHTML Action ChildSlots m
  renderOptionalKV k mv =
    case mv of
      Nothing ->
        HH.text ""
      Just v ->
        renderKV k v

  renderTuneControls :: State -> H.ComponentHTML Action ChildSlots m
  renderTuneControls state =
    case state.currentUser of
      Nothing ->
        HH.div_
          [ HH.dt_
              [ HH.text "tune" ]
          , HH.dd_
              [ renderPrintScore state ]
          ]
      Just credentials ->
        HH.div_
          [ HH.dt_
              [ HH.text "tune" ]
          , HH.dd_
              [ HH.a
                  [ safeHref $ Comments state.genre state.tuneId ]
                  [ HH.text "add comment" ]
              , renderEditAbc state credentials
              , renderDeleteAbc state credentials
              , renderPrintScore state
              ]
          ]

  -- | a user can edit the ABC if he submitted the tune or is the administrator
  -- | but this option is only available on larger screen devices (not mobiles)
  renderEditAbc :: State -> Credentials -> H.ComponentHTML Action ChildSlots m
  renderEditAbc state credentials =
    if (canEdit state.tuneMetadata credentials) && (state.deviceViewportWidth > 400) then
      HH.a
        [ safeHref $ Editor { initialAbc: Just state.tuneMetadata.abc } ]
        [ HH.text "edit tune" ]
    else
      HH.text ""

  -- | a user can delete the ABC if he submitted the tune or is the administrator
  renderDeleteAbc :: State -> Credentials -> H.ComponentHTML Action ChildSlots m
  renderDeleteAbc state credentials =
    if (canEdit state.tuneMetadata credentials) then
      HH.a
        [ css "a-internal-link"
        , HE.onClick \_ -> DeleteTune state.tuneId
        ]
        [ HH.text "delete tune" ]
    else
      HH.text ""

  renderPrintScore :: State -> H.ComponentHTML Action ChildSlots m
  renderPrintScore _state =
    HH.a
      [ css "a-internal-link"
      , HE.onClick \_ -> PrintScore
      ]
      [ HH.text "print score" ]

  renderPlayer :: State -> H.ComponentHTML Action ChildSlots m
  renderPlayer state =
    case state.tuneResult of
      Right abcTune ->
        HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")
          , HP.id "player-div"
          ]
          [ HH.slot _player unit (PC.component (toPlayable abcTune state.generateIntro state.currentBpm) state.instruments) unit HandleTuneIsPlaying ]
      Left _err ->
        HH.div_
          []

  renderTempoSlider :: State -> H.ComponentHTML Action ChildSlots m
  renderTempoSlider state =
    let
      isVisible = isRight state.tuneResult

      -- get the value from the slider result
      toTempo :: String -> Int
      toTempo s =
        fromMaybe defaultTempo.bpm $ fromString s
    in
      if (isVisible) then
        HH.div
          [ css "leftPanelComponent"
          , HP.id "tempo-slider-div"
          ]
          [ HH.text "set tempo: "
          , HH.input
              [ css "slider"
              , HE.onValueInput (HandleTempoInput <<< toTempo)
              , HP.type_ HP.InputRange
              , HP.id "tempo-slider"
              , HP.min (toNumber $ div state.originalBpm 2)
              , HP.max (toNumber $ state.originalBpm * 2)
              , HP.value (show state.currentBpm)
              , HP.disabled state.isPlaying
              ]
          , HH.div
              [ HP.class_ (H.ClassName "slider-state") ]
              [ HH.text ("  " <> state.tempoNoteLength <> "=" <> (show state.currentBpm)) ]
          ]
      else
        HH.text ""

  renderIntroButton :: State -> H.ComponentHTML Action ChildSlots m
  renderIntroButton state =
    let
      label =
        if state.generateIntro then "On"
        else "Off"
    in
      HH.div
        [ HP.id "include-intro-div" ]
        [ HH.text "Include intro when tune plays"
        , HH.button
            [ css "hoverable"
            , HP.id "include-intro-button"
            , HE.onClick \_ -> ToggleGenerateIntro
            , HP.enabled true
            ]
            [ HH.text label ]
        ]

  renderComments :: State -> H.ComponentHTML Action ChildSlots m
  renderComments state =
    let
      commentCount = length state.comments
      header =
        case commentCount of
          0 ->
            "No comments"
          1 ->
            "1 comment"
          _ ->
            show commentCount <> " comments"
    in
      HH.div_
        [ HH.text header
        -- render the comments
        , HH.div_ $ map (renderComment state) state.comments
        -- but if the load has failed, render the comments load error
        , HH.text state.commentsLoadError
        -- and render the JSON as a String (if we ever need to debug)
        , HH.text state.debugCommentsJSON
        ]

  renderComment :: State -> Comment -> H.ComponentHTML Action ChildSlots m
  renderComment state comment =
    let
      editable =
        case state.currentUser of
          Just credentials ->
            (Administrator == credentials.role) || (comment.submitter == credentials.user)
          Nothing ->
            false
    in
      HH.div
        []
        [ HH.h2
            []
            [ HH.text comment.subject ]
        , RH.render_ $ expandAllLinks comment.text
        , renderCommentMetadata comment
        --, HH.text comment.text
        , renderCommentControls state editable comment
        ]

  renderCommentMetadata :: Comment -> H.ComponentHTML Action ChildSlots m
  renderCommentMetadata comment =
    HH.div_
      [ HH.dt_
          [ HH.text "posted by" ]
      , HH.dd_
          [ HH.text comment.submitter
          , HH.text " on "
          , HH.text $ tsToDateString $ comment.timestamp
          ]
      ]

  renderCommentControls :: State -> Boolean -> Comment -> H.ComponentHTML Action ChildSlots m
  renderCommentControls state isEditable comment =
    case isEditable of
      true ->
        HH.div_
          [ HH.a
              [ css "a-internal-link"
              , HE.onClick \_ -> DeleteComment comment.commentId
              ]
              [ HH.text "delete comment" ]
          , HH.a
              [ safeHref $ Comment state.genre state.tuneId comment.commentId ]
              [ HH.text "edit comment" ]
          ]
      false ->
        HH.text ""

  renderParseError :: State -> H.ComponentHTML Action ChildSlots m
  renderParseError state =
    let
      -- instrumentCount = length state.instruments
      tuneResult = either (\r -> "tune parse error: " <> r) (const "") state.tuneResult
    in
      HH.div_
        [ HH.text tuneResult ]

  {- in case we need to track the viewport width 
  renderDebugViewportWidth ::  State -> H.ComponentHTML Action ChildSlots m
  renderDebugViewportWidth state =
    HH.div_ 
      [ HH.text "window width:"
      , HH.text (show state.deviceViewportWidth)
      ]
  -}

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      -- but the full URL won't render!!!
      -- we save the genre to Ref state to cater for instances where we want
      -- to share tune URLS and ensure that both the user sees the right genre
      -- and can carry on browsing with that genre as the default
      session <- asks _.session
      _ <- H.liftEffect $ Ref.write state.genre session.genre
      currentUser <- getUser
      baseURL <- getBaseURL
      -- corsBaseURL <- getCorsBaseURL only temporary for live server
      -- tuneMetadataResult <- requestCleanTune baseURL (asUriComponent genre) state.tuneId
      tuneMetadataResult <- requestTune baseURL state.genre state.tuneId
      let
        tuneResult =
          tuneMetadataResult >>= (\x -> lmap show $ parse x.abc)
        tuneMetadata =
          either (const state.tuneMetadata) identity tuneMetadataResult
        bpm =
          either (const defaultTempo.bpm) getBpm tuneResult
        tempoNoteLength =
          either (const "1/4") (showRatio <<< _.tempoNoteLength <<< getAbcTempo) tuneResult
      comments <- requestComments baseURL state.genre state.tuneId
      -- in case we want to trace any bad JSON returned from the server, restore this line
      -- eCommentsError <- requestCommentsStr baseURL state.genre state.tuneId

      -- set the scale of the score display according to the viewport width      
      window <- H.liftEffect HTML.window
      deviceViewportWidth <- H.liftEffect $ Window.innerWidth window

      let
        vexScale =
          if (deviceViewportWidth <= smallDeviceViewportWidthCutoff) then 0.5 else 0.8
        vexConfig = state.vexConfig { scale = vexScale }

      _ <- either (\i -> H.liftEffect $ log $ "load error: " <> i) (\_ -> pure unit) comments
      H.modify_
        ( \st -> st
            { currentUser = currentUser
            , baseURL = baseURL
            , tuneMetadata = tuneMetadata
            , tuneResult = tuneResult
            , currentBpm = bpm
            , originalBpm = bpm
            , tempoNoteLength = tempoNoteLength
            , comments = either (const []) identity comments
            , commentsLoadError = either identity (const "") comments
            --, debugCommentsJSON = either (const "") identity eCommentsError
            , deviceViewportWidth = deviceViewportWidth
            , vexConfig = vexConfig
            }
        )

      handleAction RenderScore

    Finalize -> do
      _ <- H.tell _player unit PC.StopMelody
      pure unit

    RenderScore -> do
      state <- H.get
      -- initialise the VexFlow score renderer 
      renderer <- H.liftEffect $ Score.initialiseCanvas state.vexConfig
      case state.tuneResult of
        Right tune -> do
          _ <- displayScore state renderer tune
          pure unit
        _ ->
          pure unit
      pure unit

    HandleTuneIsPlaying (PC.IsPlaying isPlaying) -> do
      _ <- H.modify_ (\st -> st { isPlaying = isPlaying })
      pure unit

    ToggleGenerateIntro -> do
      state <- H.get
      let
        generateIntro = not state.generateIntro
        newState = state { generateIntro = generateIntro }
      _ <- H.put newState
      _ <- refreshPlayerState newState
      pure unit

    HandleTempoInput bpm -> do
      state <- H.get
      _ <- H.tell _player unit PC.StopMelody
      _ <- H.modify_ (\st -> st { currentBpm = bpm })
      _ <- refreshPlayerState state
      pure unit

    DeleteTune tuneId -> do
      state <- H.get
      currentUser <- getUser
      baseURL <- getBaseURL
      case currentUser of
        Just credentials -> do
          result <- deleteTune baseURL state.genre tuneId credentials
          case result of
            Right _ -> do
              navigate Home
            Left _ ->
              pure unit
        Nothing ->
          pure unit

    DeleteComment commentId -> do
      state <- H.get
      currentUser <- getUser
      baseURL <- getBaseURL
      case currentUser of
        Just credentials -> do
          result <- deleteComment baseURL commentId credentials
          case result of
            Right _ -> do
              let
                newState = removeComment commentId state
              H.put newState
            Left _ ->
              pure unit
        Nothing ->
          pure unit
    PrintScore -> do
      state <- H.get
      _ <- H.liftEffect $ print $ getDocumentNameForPrinting state
      pure unit

-- refresh the state of the player by passing it the tune result and the tempo
refreshPlayerState
  :: ∀ o m
   . MonadAff m
  => State
  -> H.HalogenM State Action ChildSlots o m Unit
refreshPlayerState state = do
  _ <- either
    (\_ -> H.tell _player unit PC.StopMelody)
    (\abcTune -> H.tell _player unit (PC.HandleNewPlayable (toPlayable abcTune state.generateIntro state.currentBpm)))
    state.tuneResult
  pure unit

-- | convert a tune to a format recognized by the player
toPlayable :: AbcTune -> Boolean -> Int -> PlayableAbc
toPlayable abcTune generateIntro bpm =
  let
    props = defaultPlayableAbcProperties
      { tune = abcTune
      , phraseSize = 0.7
      , bpmOverride = Just bpm
      , generateIntro = generateIntro
      }
  in
    PlayableAbc props

displayScore
  :: ∀ o m
   . MonadAff m
  => State
  -> Score.Renderer
  -> AbcTune
  -> H.HalogenM State Action ChildSlots o m Unit
displayScore state renderer tune = do
  _ <- H.liftEffect $ Score.clearCanvas $ renderer
  mRendered <- H.liftEffect $ Score.renderFinalTune state.vexConfig renderer tune

  -- log any errors in attempting to produce a score  
  case mRendered of
    Just error ->
      H.liftEffect $ log (" score error: " <> error)
    _ ->
      pure unit
  pure unit

-- expand YouTube watch links to embedded iframes and geberal links to anchor tags
expandAllLinks :: String -> String
expandAllLinks =
  expandLinks <<< expandYouTubeWatchLinks

urlPreface :: State -> String
urlPreface state =
  (show state.baseURL)
    <> "/genre/"
    <> (show state.genre) -- (asUriComponent state.genre)
    <> "/tune/"
    <> tuneIdToString state.tuneId

-- | remove the comment from state
removeComment :: CommentId -> State -> State
removeComment cId state =
  let
    newComments = filter (\c -> c.commentId /= cId) state.comments
  in
    state { comments = newComments }

-- return true if the user is allowed to edit the tune
canEdit :: TuneMetadata -> Credentials -> Boolean
canEdit tuneMetadata credentials =
  credentials.role == Administrator
    || credentials.user == tuneMetadata.submitter

-- get the document name for printing - this is the tune title if it exists
-- but if not, the app name
getDocumentNameForPrinting :: State -> String
getDocumentNameForPrinting state =
  case state.tuneResult of
    Right abcTune ->
      fromMaybe "tradtunedb tune bank" $ getTitle abcTune
    _ ->
      -- shouldn't happen
      "tradtunedb tune bank"

