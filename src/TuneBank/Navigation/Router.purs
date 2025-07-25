module TuneBank.Navigation.Router where

-- | The Router Halogen Component

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (hush)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (class MonadAsk)
import Halogen as H
import Halogen.HTML as HH
import TuneBank.Navigation.Route (Route(..), routeCodec)
import TuneBank.Navigation.RouterTypes (Action(..), Input)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Toggle (resetHamburgerMenu, toggleHamburgerMenu)
import TuneBank.Page.Login as Login
import TuneBank.Page.SearchForm as SearchForm
import TuneBank.Page.AdvancedSearchForm as AdvancedSearchForm
import TuneBank.Page.GenreMenu as GenreMenu
import TuneBank.Page.Register as Register
import TuneBank.Page.ForgotUser as ForgotUser
import TuneBank.Page.ChangePassword as ChangePassword
import TuneBank.Page.Upload as Upload
import TuneBank.Page.UserList as UserList
import TuneBank.Page.Tune as Tune
import TuneBank.Page.TuneList as TuneList
import TuneBank.Page.Comment as Comment
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Credentials (Credentials)
import TuneBank.HTML.Footer (footer)
import TuneBank.HTML.Header (header)
import TuneBank.Page.Utils.Environment (getUser, getCurrentGenre)
import TuneBank.HTML.About (about)
import TuneBank.HTML.Credits (credits)
import TuneBank.HTML.ContactUs (contactUs)
import TuneBank.HTML.Help (help)
import Metronome.Container as Metronome
import Tutorial.Container as Tutorial
import Editor.Container as Editor
import Audio.SoundFont (Instrument)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))

type State =
  { route :: Maybe Route
  , genre :: Genre
  , currentUser :: Maybe Credentials
  , instruments :: Array Instrument
  }

data Query a = Navigate Route a

type ChildSlots =
  ( home :: SearchForm.Slot Unit
  , genre :: GenreMenu.Slot Unit
  , login :: Login.Slot Unit
  , register :: Register.Slot Unit
  , forgotuser :: ForgotUser.Slot Unit
  , changepassword :: ChangePassword.Slot Unit
  , upload :: Upload.Slot Unit
  , advancedsearch :: AdvancedSearchForm.Slot Unit
  , userlist :: UserList.Slot Unit
  , tune :: Tune.Slot Unit
  , tunelist :: TuneList.Slot Unit
  , comment :: Comment.Slot Unit
  , metronome :: Metronome.Slot Unit
  , tutorial :: Tutorial.Slot Unit
  , editor :: Editor.Slot Unit
  )

component
  :: ∀ m r
   . MonadAff m
  => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
  => Navigate m
  => H.Component Query Input Void m
component =
  H.mkComponent
    { initialState: \input ->
        { route: Nothing
        , genre: Scandi
        , currentUser: Nothing
        , instruments: input.instruments
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        , receive = Just <<< HandleInput
        , initialize = Just Initialize
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
      {-
      let
        foo = spy "Initialize Router route" initialRoute
      -}
      -- and, finally, we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

    HandleInput input -> do
      H.modify_ _ { instruments = input.instruments }
      pure unit
    ToggleHamburgerMenu -> do
      _ <- H.liftEffect toggleHamburgerMenu
      pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      state <- H.get
      user <- getUser
      genre <- getCurrentGenre
      -- don't re-render unnecessarily if the state is unchanged.
      when
        ( (state.route /= Just dest)
            || (state.genre /= genre)
            || (state.currentUser /= user)
        )
        do
          _ <- H.liftEffect resetHamburgerMenu
          H.modify_ _ { route = Just dest, genre = genre, currentUser = user }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = do
    let
      -- the route we give to the header defaults to Home which we use
      -- simply to highlight the menu options appropriately
      headerRoute :: Route
      headerRoute = maybe Home identity state.route
    HH.div_
      [ header state.currentUser state.genre headerRoute
      , renderRoute state
      , footer
      ]

  -- | Note - links are not well-typed.  Proxy names must also match the
  -- | child slot names AND the route codec initial URI name.
  renderRoute :: State -> H.ComponentHTML Action ChildSlots m
  renderRoute state =
    {-}
    let
      foo = spy "rendering route: " $ show state.route
    in
    -}
    case state.route of
      Just r -> case r of
        Home ->
          HH.slot (Proxy :: _ "home") unit SearchForm.component unit absurd
        Genre ->
          HH.slot (Proxy :: _ "genre") unit GenreMenu.component unit absurd
        Login ->
          HH.slot (Proxy :: _ "login") unit Login.component { currentUser: state.currentUser } absurd
        Register ->
          HH.slot (Proxy :: _ "register") unit Register.component unit absurd
        ForgotUserName ->
          HH.slot (Proxy :: _ "forgotuser") unit ForgotUser.component unit absurd
        ChangePassword ->
          HH.slot (Proxy :: _ "changepassword") unit ChangePassword.component unit absurd
        Upload ->
          HH.slot (Proxy :: _ "upload") unit Upload.component unit absurd
        AdvancedSearch ->
          HH.slot (Proxy :: _ "advancedsearch") unit AdvancedSearchForm.component unit absurd
        UserList pageParams ->
          HH.slot (Proxy :: _ "userlist") unit UserList.component { pageParams } absurd
        Tune genre tuneId ->
          HH.slot (Proxy :: _ "tune") unit Tune.component { genre, tuneId, instruments: state.instruments } absurd
        TuneList searchParams ->
          HH.slot (Proxy :: _ "tunelist") unit TuneList.component { searchParams, instruments: state.instruments } absurd
        Comments genre tuneId ->
          HH.slot (Proxy :: _ "comment") unit Comment.component { genre, tuneId, commentId: Nothing } absurd
        Comment genre tuneId commentId ->
          HH.slot (Proxy :: _ "comment") unit Comment.component { genre, tuneId, commentId: Just commentId } absurd
        Metronome ->
          HH.slot (Proxy :: _ "metronome") unit Metronome.component unit absurd
        Tutorial ->
          HH.slot (Proxy :: _ "tutorial") unit Tutorial.component { instruments: state.instruments } absurd
        Editor { initialAbc } ->
          HH.slot (Proxy :: _ "editor") unit Editor.component
            { instruments: state.instruments, initialAbc }
            absurd
        About ->
          about
        Credits ->
          credits
        ContactUs ->
          contactUs
        Help ->
          help

      Nothing ->
        HH.div_ [ HH.text "Oh no! That page wasn't found." ]
