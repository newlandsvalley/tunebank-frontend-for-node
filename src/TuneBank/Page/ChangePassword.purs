module TuneBank.Page.ChangePassword where

import Control.Monad.Reader (class MonadAsk)
import Data.Bifunctor (rmap)
import Data.Const (Const)
import Data.Either (Either(..), either, isLeft, isRight)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (invalid, validation)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<>), (/=), (<$>), (<*>), bind, discard, identity, pure, show, unit)
import TuneBank.Api.Codec.Password (OTPSubmission, ChangePassword)
import TuneBank.Api.Codec.Utils (showJsonErrorResponse)
import TuneBank.Api.Request (postChangePassword, postNewPasswordOTP)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (Validated, BaseURL)
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Page.Utils.Environment (getBaseURL)
import TuneBank.Page.Utils.UserValidation (validatePassword)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Slot = H.Slot Query Void

type State =
  { generatedOTP :: String -- the one time password generated on this page
  , userOTP :: String -- the one-time password as entered by the user
  , name :: String
  , password :: String
  , password2 :: String
  , getOTPResult :: Either String String
  , changePasswordResult :: Either String String
  }

type Query :: forall k. k -> Type
type Query = (Const Void)

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = Initialize
  | HandleUserName String
  | HandleUserOTP String
  | HandlePassword String
  | HandlePasswordConfirmation String
  | GetOTP MouseEvent
  | ChangeUserPassword MouseEvent

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
  initialState _i =
    { generatedOTP: ""
    , userOTP: ""
    , name: ""
    , password: ""
    , password2: ""
    , getOTPResult: Left ""
    , changePasswordResult: Left ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ oneTimePassword state
      , changePassword state
      ]

  oneTimePassword :: State -> H.ComponentHTML Action ChildSlots m
  oneTimePassword state =
    HH.form
      [ HP.id "loginform" ]
      [ HH.fieldset
          []
          [ HH.legend_ [ HH.text "check name" ]
          , renderUserName state
          , renderOTPSubmitButton state
          ]
      , renderGetOTPError state
      ]

  changePassword :: State -> H.ComponentHTML Action ChildSlots m
  changePassword state =
    if (isRight state.getOTPResult) then
      HH.form
        [ HP.id "changepasswordform" ]
        [ HH.fieldset
            []
            [ HH.legend_ [ HH.text "change password" ]
            , renderUserOTP state
            , renderPassword false state
            , renderPassword true state
            , renderChangePasswordButton
            ]
        , renderChangePasswordError state
        ]
    else
      HH.text ""

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

  renderUserOTP :: State -> H.ComponentHTML Action ChildSlots m
  renderUserOTP _state =
    HH.div
      [ css "textinput-div" ]
      [ HH.label
          [ css "textinput-label" ]
          [ HH.text "OTP:" ]
      , HH.input
          [ css "textinput"
          , HE.onValueInput HandleUserOTP
          , HP.value ""
          , HP.type_ HP.InputText
          ]
      ]

  renderPassword :: Boolean -> State -> H.ComponentHTML Action ChildSlots m
  renderPassword isConfirmation _state =
    let
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
            , HP.type_ HP.InputPassword
            ]
        ]

  renderOTPSubmitButton :: State -> H.ComponentHTML Action ChildSlots m
  renderOTPSubmitButton state =
    let
      buttonText = "submit"
      enabled = isLeft state.getOTPResult
      hoverable =
        either (\_ -> "hoverable") (\_ -> "unhoverable") state.getOTPResult
    in
      HH.button
        [ HE.onClick GetOTP
        , css hoverable
        , HP.enabled enabled
        ]
        [ HH.text buttonText ]

  renderGetOTPError :: State -> H.ComponentHTML Action ChildSlots m
  renderGetOTPError state =
    let
      msg = " Please enter it below along with your new password."
      text = either showJsonErrorResponse (\i -> i <> msg) state.getOTPResult
    in
      HH.div_
        [ HH.p_
            [ HH.text text ]
        ]

  renderChangePasswordButton :: H.ComponentHTML Action ChildSlots m
  renderChangePasswordButton =
    let
      buttonText = "change password"
    in
      HH.button
        [ HE.onClick ChangeUserPassword
        , css "hoverable"
        , HP.enabled true
        ]
        [ HH.text buttonText ]

  renderChangePasswordError :: State -> H.ComponentHTML Action ChildSlots m
  renderChangePasswordError state =
    let
      text = either showJsonErrorResponse identity state.changePasswordResult
    in
      HH.div_
        [ HH.p_
            [ HH.text text ]
        ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      otp <- H.liftEffect $ randomInt 100000 999999
      H.modify_ (\st -> st { generatedOTP = (show otp) })
    HandleUserName name -> do
      H.modify_ (\st -> st { name = name })
    GetOTP event -> do
      _ <- H.liftEffect $ preventDefault $ toEvent event
      state <- H.get
      baseURL <- getBaseURL
      let
        otpSubmission :: OTPSubmission
        otpSubmission =
          { name: state.name
          , otp: state.generatedOTP
          }
      -- reset any previous error text
      _ <- H.modify_ (\st -> st { getOTPResult = Left "" })
      getOTPResult <- postNewPasswordOTP baseURL otpSubmission
      _ <- H.put state { getOTPResult = getOTPResult }
      pure unit
    HandleUserOTP otp -> do
      H.modify_ (\st -> st { userOTP = otp })
    HandlePassword password -> do
      H.modify_ (\st -> st { password = password })
    HandlePasswordConfirmation password -> do
      H.modify_ (\st -> st { password2 = password })
    ChangeUserPassword event -> do
      _ <- H.liftEffect $ preventDefault $ toEvent event
      state <- H.get
      baseURL <- getBaseURL
      let
        validated = validate state

        eSubmission :: Either String ChangePassword
        eSubmission = validation
          (\errs -> Left $ foldl (<>) "" errs)
          (\cp -> Right cp)
          validated
      case eSubmission of
        Left err -> do
          let
            changePasswordResult = Left err
          H.put state { changePasswordResult = changePasswordResult }
        Right changePasswordRecord -> do
          changePasswordResult <- postChangePassword baseURL changePasswordRecord
          H.put state { changePasswordResult = changePasswordResult }
      pure unit

type ChangePassword1 =
  { name :: String
  , password :: String
  , otp :: String
  }

-- validation
validate :: State -> Validated ChangePassword
validate state =
  rmap (\cp1 -> { name: cp1.name, password: cp1.password }) $ validate1

  where

  validate1 :: Validated ChangePassword1
  validate1 =
    { name: state.name
    , password: _
    , otp: _
    }
      <$> validatePassword state.password state.password2
      <*> validateOTP state.generatedOTP state.userOTP

validateOTP :: String -> String -> Validated String
validateOTP otp confirmationOtp =
  if (otp /= confirmationOtp) then
    invalid $ pure ("Incorrect one-time-password. " <> otp <> " and " <> confirmationOtp)
  else
    pure otp

