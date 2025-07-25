module TuneBank.Page.UserList where

import Control.Monad.Reader (class MonadAsk)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude (Unit, Void, ($), (<>), (<<<), (==), bind, discard, map, pure, show, unit)
import TuneBank.Api.Codec.UsersPage (UsersPage, UserRef)
import TuneBank.Api.Request (requestUsers, deleteUser)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.UserId (UserId(..))
import TuneBank.HTML.Utils (css, tsToDateString)
import TuneBank.Navigation.Endpoint (PageParams)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getUser)
import TuneBank.HTML.PaginationRendering (renderPagination)

type Slot = H.Slot Query Void

type State =
  { currentUser :: Maybe Credentials
  , pageParams :: PageParams
  , usersResult :: Either String UsersPage
  }

data Query a =
  FetchResults a

type Input =
  { pageParams :: PageParams }

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = Initialize
  | HandleInput Input
  | DeleteUser UserId

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
        , handleQuery = handleQuery
        , receive = Just <<< HandleInput
        , initialize = Just Initialize
        , finalize = Nothing
        }
    }
  where

  initialState :: Input -> State
  initialState input =
    { currentUser: Nothing
    , pageParams: input.pageParams
    , usersResult: Left "Not started"
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    case state.usersResult of
      Left err ->
        HH.text err
      Right usersPage ->
        case (length usersPage.users) of
          0 ->
            HH.text "no users found"
          _ ->
            HH.div_
              [ HH.h4
                  [ css "center" ]
                  [ HH.text
                      ( "user list page "
                          <> show usersPage.pagination.page
                          <> " of "
                          <> show usersPage.pagination.maxPages
                      )
                  ]
              , renderUserList state usersPage.users
              , renderPagination (UserList state.pageParams) usersPage.pagination
              ]

  renderUserList :: State -> Array UserRef -> H.ComponentHTML Action ChildSlots m
  renderUserList _state users =
    let
      f userRef =
        tableRow userRef
    in
      HH.table_ $
        map f users
    where
    tableRow userRef =
      HH.tr_
        [ HH.td_
            [ HH.text userRef.name ]
        , HH.td_
            [ HH.text userRef.email ]
        , HH.td_
            [ HH.text userRef.role ]
        , HH.td_
            [ HH.text userRef.valid ]
        , HH.td_
            [ HH.text $ tsToDateString userRef.timestamp ]
        , HH.td_
            [ renderDeleteUser userRef ]
        ]

  -- | only the administrator can see this page, and we'll give him the option 
  -- | of deleting a user if it's not been validated 
  renderDeleteUser :: UserRef -> H.ComponentHTML Action ChildSlots m
  renderDeleteUser userRef =
    if (userRef.valid == "N") then
      HH.a
        [ css "a-internal-link"
        , HE.onClick \_ -> DeleteUser (UserId userRef.name)
        ]
        [ HH.text "delete user" ]
    else
      HH.text ""

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      mUser <- getUser
      _ <- H.modify (\st -> st { currentUser = mUser })
      _ <- handleQuery (FetchResults unit)
      pure unit
    HandleInput input -> do
      H.modify_ (\st -> st { pageParams = input.pageParams })
      _ <- handleQuery (FetchResults unit)
      pure unit
    DeleteUser userId -> do
      baseURL <- getBaseURL
      currentUser <- getUser
      case currentUser of
        Just credentials -> do
          result <- deleteUser baseURL userId credentials
          case result of
            Right _ -> do
              _ <- handleQuery (FetchResults unit)
              pure unit
            Left _err -> do
              -- let 
              -- _foo = spy "error on deleting user" err
              pure unit
        Nothing ->
          pure unit
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    FetchResults next -> do
      state <- H.get
      -- live server testing only baseURL <- getCorsBaseURL
      baseURL <- getBaseURL
      usersResult <- requestUsers baseURL state.currentUser state.pageParams
      H.modify_ (\st -> st { usersResult = usersResult })
      pure (Just next)
