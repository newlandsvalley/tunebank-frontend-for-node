module TuneBank.HTML.Utils where

import Prelude
import TuneBank.Navigation.Route (Route, routeCodec)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Data.Int (toNumber)
import Data.String (length, take)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Time.Duration (Milliseconds(..))
import Data.Rational (Rational, numerator, denominator)
import Data.Formatter.DateTime (formatDateTime)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Routing.Duplex (print)

-- | I get annoyed writing `class_ $ ClassName "..."` over and over again. This small utility saves
-- | a few characters all over our HTML.
css :: forall r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName

-- ditto for the SVG element
svgcss :: forall r i. String -> HH.IProp (class :: String | r) i
svgcss = SA.class_ <<< HH.ClassName

-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
safeHref :: forall r i. Route -> HH.IProp (href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
debugHref :: Route -> String
debugHref = print routeCodec

-- | Sometimes we need to deal with elements which may or may not exist. This function lets us
-- | provide rendering for the element if it exists, and renders an empty node otherwise.
maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

-- | PureScript is a strict language. If we want to conditionally display an element, then we
-- | should hide the evaluation behind a function, which won't be evaluated right away, in order
-- | to minimize the work performed each render.
whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""

truncateTo :: Int -> String -> String
truncateTo maxLen s =
  if (maxLen >= length s) then
    s
  else
    (take maxLen s) <> "..."

tsToDateString :: Int -> String
tsToDateString ts =
  let
    mInstant = instant $ Milliseconds $ (toNumber ts) * 1000.0
    -- mInstant = instant $ Milliseconds $ readFloat tsString
    dateTime = maybe (bottom) (toDateTime) mInstant
  in
    either (const "bad date") identity $ formatDateTime "DD MM YY" dateTime

-- render key-value pairs as rows in a definition list
renderKV :: ∀ a s m. String -> String -> H.ComponentHTML a s m
renderKV k v =
  HH.div_
    [ HH.dt_
        [ HH.text k ]
    , HH.dd_
        [ HH.text v ]
    ]

showRatio :: Rational -> String
showRatio r =
  (show $ numerator r) <> "/" <> (show $ denominator r)
