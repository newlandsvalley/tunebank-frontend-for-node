module Test.Main where

import Prelude

import Data.Array (length) as A
import Data.Bifunctor (rmap)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Routing.Duplex (print)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Request (checkUser, requestComments, requestTune, requestTuneAbc, requestTuneSearch, requestUsers)
import TuneBank.Api.Codec.Utils (unsafeEncodeURIComponent, unsafeDecodeURIComponent)
import TuneBank.Data.Credentials (Role(..), Credentials)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.TuneId (TuneId(..))
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.Api.Codec.Tune (TuneMetadata)
import TuneBank.Navigation.Endpoint (PageParams)
import TuneBank.Navigation.Route (Route(..), routeCodec)
import TuneBank.Navigation.SearchParams (SearchParams, defaultSearchParams, parseParams)


baseURL :: BaseURL
-- production server
baseURL = BaseURL "http://localhost:8080"

sampleTune :: TuneId
sampleTune =
  TuneId "Elverumspols"

sampleCommentedTune :: TuneId
sampleCommentedTune =
  TuneId "Griffenfeldt"

sampleUncommentedTune :: TuneId
sampleUncommentedTune =
  TuneId "Fastan"

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
  , pass : "changeit"
  , role : Administrator
  }

unknownUser :: Credentials
unknownUser =
  { user : "unknown"
  , pass : "xyz123"
  , role : NormalUser
  }


main :: Effect Unit
main = runSpecAndExitProcess [ specReporter] do
  describe "tunebank frontend" do
    apiSpec
    codecSpec
    routesSpec

-- this spec merely tests the validity of the endoint API by mimicing the Request API in Node
apiSpec :: Spec Unit
apiSpec =
  describe "Endpoint API" do
    it "gets tune" do
      eRes :: (Either String TuneMetadata) <- requestTune baseURL Scandi sampleTune
      case eRes of 
        Left err -> fail err 
        Right metadata -> do
          metadata.title `shouldEqual` "Elverumspols"
    
    it "gets tune ABC" do
      eRes :: (Either String String) <- requestTuneAbc baseURL Scandi sampleTune
      case eRes of 
        Left err -> fail err 
        Right abc -> do
          abc `shouldSatisfy` contains (Pattern "Elverumspols")     

    it "does simple search" do
      response <- requestTuneSearch baseURL Scandi simpleSearch
      -- these tests below now fail
      rmap (\tunes -> tunes.pagination.size) response `shouldEqual` (Right 15)
      rmap (\tunes -> tunes.pagination.page) response `shouldEqual` (Right 1) 
      rmap (\tunes -> tunes.pagination.maxPages) response `shouldEqual` (Right 2) 

    it "does complex search" do
      response <- requestTuneSearch baseURL Scandi complexSearch
      rmap (\tunes -> tunes.pagination.size) response `shouldEqual` (Right 15) 
      rmap (\tunes -> tunes.pagination.maxPages) response `shouldEqual` (Right 1) 

    it "gets users" do
      response  <- requestUsers baseURL (Just adminUser) page1
      rmap (\users -> users.pagination.size) response `shouldEqual` (Right 15) 
      rmap (\users -> users.pagination.page) response `shouldEqual` (Right 1) 
    
    it "checks good user" do
      userCheck <- checkUser baseURL adminUser
      userCheck `shouldEqual` (Right "administrator") 
    
    it "checks bad user" do
      userCheck <- checkUser baseURL unknownUser
      userCheck `shouldSatisfy` isLeft 
    
    it "gets tune comments" do
      comments <- requestComments baseURL Scandi sampleCommentedTune
      rmap A.length comments `shouldEqual` (Right 1) 

    it "gets tune empty comments" do
      comments <- requestComments baseURL Scandi sampleUncommentedTune
      rmap A.length comments `shouldEqual` (Right 0) 

codecSpec :: Spec Unit
codecSpec =
  describe "Codecs" do
    it "decodes tune name" do
      let
        sampleTuneName = "andet brudestykke"
      shouldEqual sampleTuneName
        (unsafeDecodeURIComponent $ unsafeEncodeURIComponent sampleTuneName)
    it "parses search params" do
      let
        expected = defaultSearchParams { rhythm = Just "reel", key = Just "BMin" }
      shouldEqual (Right expected) (parseParams "rhythm=reel&key=BMin")

routesSpec :: Spec Unit
routesSpec =
  describe "Routes" do
    it " has Home route" do
      shouldEqual "/" (print routeCodec Home)
    it "has Login route" do
      shouldEqual "/login" (print routeCodec Login)
    it "has TuneList route" do
      shouldEqual "/tunelist?sort=alpha&page=1" (print routeCodec $
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
    it "has UserList route" do
      shouldEqual "/users?page=1" (print routeCodec $ UserList { page : 1 })

