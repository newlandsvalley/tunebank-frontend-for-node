module TuneBank.Page.ValidateRegistration where

-- | validate the registration UUID sent to the user via email


import Prelude (Unit, Void, ($), bind, discard, pure, unit)
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TuneBank.Api.Request (checkRegistration)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.Uuid (Uuid)
import TuneBank.Page.Utils.Environment (getBaseURL)
import TuneBank.HTML.Utils (css)

type Slot = H.Slot (Const Void) Void

type Input =
  { uuid :: Uuid }

type State =
  { uuid :: Uuid 
  , validity :: Either String String
  }

type Query :: forall k. k -> Type
type Query = (Const Void)

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = Initialize
  | HandleContinueToLogin

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
    { uuid: input.uuid
    , validity : Left "code not recognised"
    }

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      baseURL <- getBaseURL
      validity <- checkRegistration baseURL state.uuid
      H.modify_ (\st -> st { validity = validity })      
      pure unit 
    HandleContinueToLogin -> do 
      _ <- navigate $ Login
      pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =  
    HH.div_
      [ HH.div
          [ HP.id "registrationvalidation" ]
          [ HH.h1_
              [ HH.text "User Registration Validation" ]
          , nextStep state
          ]
      ]

  nextStep :: State -> H.ComponentHTML Action ChildSlots m
  nextStep state =  
    case state.validity of 
      Left _ -> 
        HH.div 
          [ HP.id "registration-validation-item-center" ]          
          [ HH.text "code not recognised" ]
      Right _ ->
        HH.div
          [ HP.id "registration-validation-item-center" ]
          [ HH.div 
              [ HP.id "registration-validation-text"]
              [ HH.text "validated!"]
          , renderContinueButton
          ]
        

  renderContinueButton :: H.ComponentHTML Action ChildSlots m
  renderContinueButton =
    HH.button
      [ HE.onClick \_ -> HandleContinueToLogin
      , css "hoverable"
      , HP.enabled true
      ]
      [ HH.text "go to login" ]


    