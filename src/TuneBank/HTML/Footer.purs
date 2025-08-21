module TuneBank.HTML.Footer (footer) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Navigation.Route (Route(..))

footer :: forall i p. HH.HTML i p
footer =
  HH.footer
    [ css "footer" ]
    [ HH.div_
        [ HH.div
            [ css "mainlogo" ]
            [ HH.span
                [ css "mainlogo-prefix" ]
                [ HH.text "tunebank" ]
            , HH.span
                [ css "mainlogo-suffix" ]
                [ HH.text ".org.uk" ]
            ]
        ]
    , HH.span
        [ css "footer-menu" ]
        [ HH.nav
            [ css "footer-nav" ]
            [ navItem About
                [ HH.text "about" ]
            ]
        , HH.nav
            [ css "footer-nav" ]
            [ navItem Credits
                [ HH.text "credits" ]
            ]
        , HH.nav
            [ css "footer-nav" ]
            [ navItem Help
                [ HH.text "help" ]
            ]
        , HH.nav
            [ css "footer-nav" ]
            [ navItem Tutorial
                [ HH.text "ABC tutorial" ]
            ]
        , HH.nav
            [ css "footer-nav" ]
            [ navItem ContactUs
                [ HH.text "contact us" ]
            ]
        , HH.nav
            [ css "footer-nav" ]
            [ externalLink "https://tunebank.org.uk:8605/"
                [ HH.text "share-a-tune" ]
            ]
        ]
    ]

  where

  -- | a navigation item on this site available at any time
  navItem :: Route -> Array (HH.HTML i p) -> HH.HTML i p
  navItem r html =
    HH.div
      [ css "nav-item" ]
      [ HH.a
          [ safeHref r
          ]
          html
      ]

  -- | a link to an external site
  externalLink :: String -> Array (HH.HTML i p) -> HH.HTML i p
  externalLink link html =
    HH.div
      [ css "nav-item" ]
      [ HH.a
          [ HP.href link
          ]
          html
      ]

