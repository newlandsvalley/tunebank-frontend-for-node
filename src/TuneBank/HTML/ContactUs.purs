module TuneBank.HTML.ContactUs (contactUs) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TuneBank.HTML.Utils (css)

contactUs :: forall i p. HH.HTML i p
contactUs =
  HH.div
    [ HP.id "contactusdiv" ]
    [ HH.h2_
        [ HH.text "Please get in touch" ]
    , HH.p_
        [ HH.text "Perhaps you have questions about the site or suggestions for improvements. If so, contact "
        , HH.address            
            [ css "contactus"]
            [ HH.a
              [ HP.href "mailto:john.watson@gmx.co.uk" ]
              [ HH.text "John Watson" ]
            ]
        , HH.text " and I'd be delighted to hear from you."
        ]
    ]
