module TuneBank.HTML.Error (notFoundPage) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


{-}
errorPage :: Int -> String -> forall i p. HH.HTML i p
errorPage errorCode error =
  HH.div
    [ HP.id "errordiv" ]
    [ HH.h2_
        [ HH.text $ show errorCode ]
    , HH.p_
        [ HH.text error ]
    ]
-}

notFoundPage :: forall i p. HH.HTML i p
notFoundPage = 
  HH.div
    [ HP.id "errordiv" ]
    [ HH.h2_
        [ HH.text "404" ]
    , HH.p_
        [ HH.text "Sorry - this page cannot be found" ]
    ]
