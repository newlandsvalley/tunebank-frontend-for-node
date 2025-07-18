module TuneBank.Page.ForgotUser where

import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Prelude (Unit, Void, ($), bind, const, pure, unit)
import TuneBank.Api.Codec.Utils (showJsonErrorResponse)
import TuneBank.Api.Request (postEmail)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Page.Utils.Environment (getBaseURL)

type Slot = H.Slot Query Void

type State =
  { email :: String
  , forgotUserResult :: Either String String
  }

type Query :: forall k. k -> Type
type Query = (Const Void)

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = HandleEmail String
  | ForgotUser MouseEvent

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
        , initialize = Nothing
        , finalize = Nothing
        }
    }
  where

  initialState :: i -> State
  initialState _i =
    { email: ""
    , forgotUserResult: Left ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ forgotUser state
      ]

  forgotUser :: State -> H.ComponentHTML Action ChildSlots m
  forgotUser state =
    HH.form
      [ HP.id "loginform" ]
      [ HH.fieldset
          []
          [ HH.legend_ [ HH.text "Get user name" ]
          , renderEmail state
          , renderSubmitButton
          ]
      , renderForgotUserError state
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

  renderSubmitButton :: H.ComponentHTML Action ChildSlots m
  renderSubmitButton =
    let
      buttonText = "submit"
    in
      HH.button
        [ HE.onClick ForgotUser
        , css "hoverable"
        , HP.enabled true
        ]
        [ HH.text buttonText ]

  renderForgotUserError :: State -> H.ComponentHTML Action ChildSlots m
  renderForgotUserError state =
    let
      msg = " An email message has been sent to you with your user name."
      text = either showJsonErrorResponse (const msg) state.forgotUserResult
    in
      HH.div_
        [ HH.text text ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    HandleEmail email -> do
      H.modify_ (\st -> st { email = email })
    ForgotUser event -> do
      _ <- H.liftEffect $ preventDefault $ toEvent event
      state <- H.get
      baseURL <- getBaseURL
      -- reset any previous error text
      _ <- H.modify_ (\st -> st { forgotUserResult = Left "" })
      forgotUserResult <- postEmail baseURL state.email
      _ <- H.put state { forgotUserResult = forgotUserResult }
      pure unit
