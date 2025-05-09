module Test.Main where

import Prelude
import Control.Monad.Free (Free)
import Data.Array (length) as A
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Bifunctor (rmap)
import Effect (Effect)
import Test.Unit (Test, TestF, suite, test, failure, success)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Routing.Duplex (print)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.Data.TuneId (TuneId(..))
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Credentials (Role(..), Credentials)
import TuneBank.Api.Request (requestTune, requestTuneAbc, requestTuneSearch, checkUser, 
       requestUsers, requestComments)
import TuneBank.Api.Codec.Utils ( unsafeEncodeURIComponent, unsafeDecodeURIComponent) 
import TuneBank.Navigation.Endpoint (PageParams)
import TuneBank.Navigation.Route (Route(..), routeCodec)
import TuneBank.Navigation.SearchParams (SearchParams, defaultSearchParams, parseParams)

assertRight :: forall a b. Either a b -> Test
assertRight either =
  case either of
    Left _ -> failure ("Right expected")
    Right _ -> success

baseURL :: BaseURL
-- production server
baseURL = BaseURL "http://localhost"

sampleTune :: TuneId
sampleTune =
  TuneId "antara"

sampleCommentedTune :: TuneId
sampleCommentedTune =
  -- production server
  -- TuneId $ { title : "andet+brudestykke", tuneType: "marsch" }
  TuneId "vals+a+lulu"

simpleSearch :: SearchParams
simpleSearch =
  defaultSearchParams

complexSearch :: SearchParams
complexSearch =
  simpleSearch { key = Just "Dmaj", rhythm = Just "polska"}

page1 :: PageParams
page1 =
  { page: 1 }

adminUser :: Credentials
adminUser =
  { user : "administrator"
  , pass : "h0rsf0rth"
  , role : Administrator
  }
  {- production server
    { user : "administrator"
    , pass : "b*"
    , role : Administrator
    }
  -}


unknownUser :: Credentials
unknownUser =
  { user : "unknown"
  , pass : "xyz123"
  , role : NormalUser
  }

{-}
samplePredicateString :: String
samplePredicateString =
  "T=antara-reel&abc= F G A"

samplePredicate :: SearchPredicate
samplePredicate =
  [ { key: "R", value: "polska" }
  , { key: "K", value: "Dmaj" }
  ]
-}

main :: Effect Unit
main = runTest do
  apiSuite
  codecSuite
  routesSuite

apiSuite :: Free TestF Unit
apiSuite =
  suite "Endpoint API" do
    test "get tune" do
      resource <- requestTune baseURL Irish sampleTune
      assertRight resource
    test "get tune ABC" do
      resource <- requestTuneAbc baseURL Irish sampleTune
      assertRight resource
    test "simple search" do
      response <- requestTuneSearch baseURL "irish" simpleSearch
      -- these tests below now fail
      Assert.equal (Right 15) $ rmap (\tunes -> tunes.pagination.size) response
      Assert.equal (Right 1) $  rmap (\tunes -> tunes.pagination.page) response
      Assert.equal (Right 5) $  rmap (\tunes -> tunes.pagination.maxPages) response
    test "complex search" do
    -- these tests below now fail
      response <- requestTuneSearch baseURL "scandi" complexSearch
      Assert.equal (Right 15) $ rmap (\tunes -> tunes.pagination.size) response
      Assert.equal (Right 3) $  rmap (\tunes -> tunes.pagination.maxPages) response
    test "get users" do
      -- these tests below now fail
      response  <- requestUsers baseURL (Just adminUser) page1
      Assert.equal (Right 15) $  rmap (\users -> users.pagination.size) response
      Assert.equal (Right 1) $  rmap (\users -> users.pagination.page) response
    test "check good user" do
      userCheck <- checkUser baseURL adminUser
      Assert.equal (Right "user is valid") userCheck
    test "check bad user" do
      userCheck <- checkUser baseURL unknownUser
      Assert.equal (Right "The supplied authentication is invalid") userCheck
    test "get tune comments" do
      comments <- requestComments baseURL Scandi sampleCommentedTune
      Assert.equal (Right 1) $ rmap A.length comments
    test "get tune empty comments" do
      comments <- requestComments baseURL Scandi sampleTune
      Assert.equal (Right 0) $ rmap A.length comments

codecSuite :: Free TestF Unit
codecSuite =
  suite "Codecs" do
    test "tune name" do
      let
        sampleTuneName = "andet brudestykke"
      Assert.equal sampleTuneName
        (unsafeDecodeURIComponent $ unsafeEncodeURIComponent sampleTuneName)
    test "parse search params" do
      let
        expected = defaultSearchParams { rhythm = Just "reel", key = Just "BMin" }
      Assert.equal (Right expected) (parseParams "rhythm=reel&key=BMin")

routesSuite :: Free TestF Unit
routesSuite =
  suite "Routes" do
    test "Home" do
      Assert.equal "/" (print routeCodec Home)
    test "Login" do
      Assert.equal "/login" (print routeCodec Login)
    test "TuneList" do
      Assert.equal "/tunelist?sort=alpha&page=1" (print routeCodec $
        TuneList
             { key : Nothing
             , rhythm : Nothing
             , title : Nothing
             , source : Nothing
             , origin : Nothing
             , composer : Nothing
             , transcriber : Nothing
             , submitter : Nothing
             , page: 1
             , sort : "alpha" })
    test "UserList" do
      Assert.equal "/users?page=1" (print routeCodec $ UserList { page : 1 })

