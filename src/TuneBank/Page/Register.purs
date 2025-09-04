module TuneBank.Page.Register where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String (contains, length)
import Data.String.Common (null)
import Data.String.Pattern (Pattern(..))
import Data.Validation.Semigroup (invalid, validation)
import Effect.Aff.Class (class MonadAff)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TuneBank.Api.Codec.Register (Submission, defaultSubmission)
import TuneBank.Api.Codec.Utils (showJsonErrorResponse)
import TuneBank.Api.Request (postNewUser)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (Validated, BaseURL)
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Page.Utils.Environment (getBaseURL)
import TuneBank.Page.Utils.UserValidation (validatePassword)

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { submission :: Submission
  , userRegisterResult :: Either String String -- result from server
  , errorText :: String -- validation errors
  , showPasswords :: Boolean  -- do we show the user input for her passwords?
  }

type Query :: forall k. k -> Type
type Query = (Const Void)

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = Initialize
  | HandleUserName String
  | HandleEmail String
  | HandlePassword String
  | HandleShowPasswords Boolean
  | HandlePasswordConfirmation String
  | RegisterUser MouseEvent

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
    { submission: defaultSubmission
    , userRegisterResult: Left ""
    , errorText: ""
    , showPasswords : false
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.form
      [ HP.id "registrationform" ]
      [ HH.fieldset
          []
          [ HH.legend_ [ HH.text "Register" ]
          , renderUserName state
          , renderEmail state
          , renderPassword false state
          , renderPassword true state
          , renderShowPasswordsOption
          , renderRegisterButton
          ]
      , renderRegisterError state
      ]

  renderUserName :: State -> H.ComponentHTML Action ChildSlots m
  renderUserName _state =
    HH.div
      [ css "textinput-div" ]
      [ HH.label
          [ css "textinput-label" ]
          [ HH.text "name:" ]
      , HH.input
          [ css "textinput"
          , HE.onValueInput HandleUserName
          , HP.value ""
          , HP.type_ HP.InputText
          ]
      ]

  renderEmail :: State -> H.ComponentHTML Action ChildSlots m
  renderEmail _state =
    HH.div
      [ css "textinput-div" ]
      [ HH.label
          [ css "textinput-label" ]
          [ HH.text "email:" ]
      , HH.input
          [ css "textinput"
          , HE.onValueInput HandleEmail
          , HP.value ""
          , HP.type_ HP.InputText
          ]
      ]

  renderPassword :: Boolean -> State -> H.ComponentHTML Action ChildSlots m
  renderPassword isConfirmation state =
    let
      inputType = 
        if (state.showPasswords) then HP.InputText else HP.InputPassword
      action =
        if isConfirmation then
          HandlePasswordConfirmation
        else
          HandlePassword
      label =
        if isConfirmation then
          "confirm password:"
        else
          "password:"
    in
      HH.div
        [ css "textinput-div" ]
        [ HH.label
            [ css "textinput-label" ]
            [ HH.text label ]
        , HH.input
            [ css "textinput"
            , HE.onValueInput action
            , HP.value ""
            , HP.type_ inputType
            ]
        ]


  renderShowPasswordsOption :: H.ComponentHTML Action ChildSlots m
  renderShowPasswordsOption =
    HH.div
      [ css "register-checkbox-div" ]
      [ HH.input
          [ css "checkbox"
          , HE.onChecked HandleShowPasswords
          , HP.checked false
          , HP.type_ HP.InputCheckbox
          ]
      , HH.text "show passwords"
      ]

  renderRegisterButton :: H.ComponentHTML Action ChildSlots m
  renderRegisterButton =
    HH.button
      [ HE.onClick RegisterUser
      , css "hoverable"
      , HP.enabled true
      ]
      [ HH.text "register user" ]

  renderRegisterError :: State -> H.ComponentHTML Action ChildSlots m
  renderRegisterError state =
    let
      registrationOK :: String -> String
      registrationOK msg =
        msg
          <> " The last part of the registration process is to reply to an"
          <>
            " email confirmation message that has been sent to you."
      registrationText = either showJsonErrorResponse registrationOK state.userRegisterResult
    in
      HH.div_
        [ HH.text state.errorText
        , HH.text registrationText
        ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      pure unit
    HandleUserName name -> do
      state <- H.get
      let
        newSubmission = state.submission { name = name }
      H.modify_ (\st -> st { submission = newSubmission })
    HandleEmail email -> do
      state <- H.get
      let
        newSubmission = state.submission { email = email }
      H.modify_ (\st -> st { submission = newSubmission })
    HandlePassword password -> do
      state <- H.get
      let
        newSubmission = state.submission { password = password }
      H.modify_ (\st -> st { submission = newSubmission })
    HandleShowPasswords showPasswords -> do 
      _ <- H.modify_ (\st -> st { showPasswords = showPasswords })
      pure unit
    HandlePasswordConfirmation password -> do
      state <- H.get
      let
        newSubmission = state.submission { password2 = password }
      H.modify_ (\st -> st { submission = newSubmission })
    RegisterUser event -> do
      _ <- H.liftEffect $ preventDefault $ toEvent event
      state <- H.get
      baseURL <- getBaseURL
      -- reset any previous error text
      H.modify_ (\st -> st { userRegisterResult = Left "" })
      let
        validated = validate state.submission
        newState = validation
          (\errs -> state { errorText = foldl (<>) "" errs })
          ( \submission -> state
              { submission = submission
              , errorText = ""
              , userRegisterResult = Left ""
              }
          )
          validated

      if (null newState.errorText) then do
        userRegisterResult <- postNewUser newState.submission baseURL
        _ <- H.put newState { userRegisterResult = userRegisterResult }
        pure unit
      else do
        _ <- H.put newState
        pure unit
      pure unit

-- validation
validate :: Submission -> Validated Submission
validate submission =
  { name: _
  , email: _
  , password: _
  , password2: submission.password2
  , refererUrl: submission.refererUrl
  }
    <$> validateName submission.name
    <*> validateEmail submission.email
    <*> validatePassword submission.password submission.password2

validateName :: String -> Validated String
validateName name =
  if (0 < length name) then
    pure name
  else
    invalid $ pure ("Invalid name. ")

validateEmail :: String -> Validated String
validateEmail email =
  if (contains (Pattern "@") email) then
    pure email
  else
    invalid $ pure ("Invalid email. ")
