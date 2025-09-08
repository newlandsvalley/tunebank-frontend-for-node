module TuneBank.Page.Upload where

import Control.Monad.Reader (class MonadAsk)
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Abc.Utils (getTitle)
import Data.Abc.Parser (parse)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.Tuple (Tuple(..))
import Data.String.CodeUnits (contains, takeRight)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Halogen as H
import Halogen.FileInputComponent as FIC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<>), (==), bind, const, discard, identity, pure, show, unit)
import TuneBank.Api.Codec.Utils (showJsonErrorResponse)
import TuneBank.Api.Request (postTune)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (TuneId(..))
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(Tune))
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre, getUser)
import Type.Proxy (Proxy(..))

-- type Slot = H.Slot Query Void
type Slot = H.Slot Query Void

type State =
  { genre :: Genre
  , currentUser :: Maybe Credentials
  , baseURL :: BaseURL
  , fileName :: Maybe String
  , abc :: String
  , errorText :: String
  }

data Query a =
  PostTune a

type ChildSlots =
  (abcfile :: FIC.Slot Unit)

_abcfile = Proxy :: Proxy "abcfile"

abcFileInputCtx :: FIC.Context
abcFileInputCtx =
  { componentId: "abcinput"
  , isBinary: false
  , prompt: "choose file"
  , accept: mediaType (MediaType ".abc")
  }

data Action
  = Initialize
  | HandleABCFile FIC.Message
  | UploadFile MouseEvent

component
  :: ∀ i o m r
   . MonadAff m
  => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
  => Navigate m
  => H.Component Query i o m
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

  initialState :: i -> State
  initialState _ =
    { genre: Scandi
    , currentUser: Nothing
    , baseURL: BaseURL ""
    , fileName: Nothing
    , abc: ""
    , errorText: ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.form
          [ HP.id "uploadform" ]
          [ HH.fieldset
              []
              [ HH.legend_ [ HH.text "Upload Tune" ]
              , renderAdvisoryText state
              , renderSelectFile state
              , renderUploadButton state
              ]
          , renderError state
          ]
      ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      baseURL <- getBaseURL
      mUser <- getUser
      genre <- getCurrentGenre
      _ <- H.modify
        ( \state -> state
            { genre = genre
            , currentUser = mUser
            , baseURL = baseURL
            }
        )
      pure unit
    HandleABCFile (FIC.FileLoaded filespec) -> do
      let 
        abc = ensureTermination filespec.contents
      _ <- H.modify
        ( \st -> st
            { fileName = Just filespec.name
            , abc = abc
            , errorText = ""
            }
        )
      pure unit
    UploadFile event -> do
      _ <- H.liftEffect $ preventDefault $ toEvent event
      _ <- handleQuery (PostTune unit)
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    PostTune next -> do
      state <- H.get
      baseURL <- getBaseURL
      -- basic validation - the ABC tune parses and has a title
      -- and there are no chords in the body
      let
        eTuneTitle = validateTune state.abc
      case (Tuple state.currentUser eTuneTitle) of
        (Tuple (Just credentials) (Right _title)) -> do
          postResult <- postTune state.abc baseURL state.genre credentials
          let
            errorText = either identity (const "") postResult
          H.modify_ (\st -> st { errorText = errorText })
          case postResult of
            -- we posted OK and got a good response with the title in the response
            Right resultStr -> do
              _ <- navigate $ Tune state.genre (TuneId resultStr)
              pure (Just next)
            Left err -> do
              H.modify_ (\st -> st { errorText = showJsonErrorResponse err })
              pure (Just next)
        (Tuple _ (Left err)) -> do
          H.modify_ (\st -> st { errorText = err })
          pure (Just next)
        _ ->
          pure (Just next)

renderAdvisoryText :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderAdvisoryText state =
  let
    text1 =
      "Upload an ABC file to the "
        <> (show state.genre)
        <> " genre. (The file extension should be "
        <>
          ".abc although .txt is also allowed.)"
    text2 =
      "The file should contain a single tune without chord symbols."
  in
    HH.div_
      [ HH.p_
          [ HH.text text1 ]
      , HH.p_
          [ HH.text text2 ]
      ]

renderSelectFile :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderSelectFile _state =
  HH.div
    [ css "fileinput-div"
    , HP.id "uploadselectfile"
    ]
    [ HH.label
        [ css "fileinput-label" ]
        [ HH.text "load ABC file:" ]
    , HH.slot _abcfile unit (FIC.component abcFileInputCtx) unit HandleABCFile
    ]

renderUploadButton :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderUploadButton _state =
  HH.button
    [ HE.onClick UploadFile
    , css "hoverable"
    , HP.enabled true
    ]
    [ HH.text "upload" ]

renderError :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderError state =
  HH.div
    []
    [ HH.text state.errorText ]

validateTune :: String -> Either String String
validateTune abc =
  if (textContainsQuotes abc) then
    Left
      ( "Embedded double quotes are not supported" <>
          " - i.e. ABC containing chord symbols is rejected"
      )
  else
    validTuneTitle abc

validTuneTitle :: String -> Either String String
validTuneTitle abc =
  case parse abc of
    Left err -> Left ("invalid ABC: " <> show err)
    Right tune ->
      maybe (Left "No tune title present") (\t -> Right t) $ getTitle tune

-- | check whether the parsed ABC text is invalid for submission to the server
-- | at the moment, just one check is performed - the server doesn't accept
-- | embedded double quotes - i.e. it rejects chords
textContainsQuotes :: String -> Boolean
textContainsQuotes text =
  contains (Pattern "\"") text


-- | check that the last character is a carriage return and add it if it's not present
ensureTermination :: String -> String 
ensureTermination abc = 
  if ((takeRight 1 abc) == "\n")
    then abc 
    else (abc <> "\n")

