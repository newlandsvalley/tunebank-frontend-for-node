module TuneBank.Api.Codec.Utils
  ( containsDigit
  , showJsonErrorResponse
  , unsafeEncodeURIComponent
  , unsafeDecodeURIComponent
  , safeSlice) where


import Data.Argonaut (Json, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Prelude (($), (>=), (<=), (&&), bind, identity, pure)
import Data.Either (Either(..), either)
import Data.Maybe (fromJust)
import Data.Foldable (any)
import Data.String (null)
import Data.String.CodeUnits (slice)
import Data.String.CodePoints (CodePoint, codePointFromChar, toCodePointArray)
import Partial.Unsafe (unsafePartial)
import JSURI (decodeURIComponent, encodeURIComponent)

unsafeEncodeURIComponent :: String -> String
unsafeEncodeURIComponent s = 
  unsafePartial $ fromJust $ encodeURIComponent s

unsafeDecodeURIComponent :: String -> String
unsafeDecodeURIComponent s = 
  unsafePartial $ fromJust $ decodeURIComponent s

safeSlice :: Int -> Int -> String -> String
safeSlice from to str =
  slice from to str

isDigit :: CodePoint -> Boolean
isDigit cp =
  cp >= codePointFromChar '0' && cp <= codePointFromChar '9'

-- | return true if the string contains any digit in the range 0-9
containsDigit :: String -> Boolean
containsDigit s =
  any isDigit $ toCodePointArray s

showJsonErrorResponse :: String -> String 
showJsonErrorResponse jsonString = 
  if (null jsonString)
    then ""
  else
    case (parseJson jsonString) of 
      Left err -> 
        printJsonDecodeError err 
      Right json ->
        either printJsonDecodeError identity $ decodeErrorResponse json

  where
  -- | decode a JSON error response from the tunebank server
  decodeErrorResponse :: Json -> Either JsonDecodeError String
  decodeErrorResponse json = do
    obj <-  decodeJson json
    message <- obj .: "message"
    pure message

