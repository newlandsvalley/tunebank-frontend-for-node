module TuneBank.HTML.Credits (credits) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

credits :: forall i p. HH.HTML i p
credits =
  HH.div
    [ HP.id "creditsdiv" ]
    [ HH.p_
        [ HH.text "TradTuneDb uses "
        , HH.a
            [ HP.href "http://abcnotation.com/" ]
            [ HH.text "ABC notation" ]
        , HH.text " and so obviously owes a tremendous debt to Chris Walshaw who developed it. It uses the "
        , HH.a
            [ HP.href "https://github.com/newlandsvalley/tunebank-node" ]
            [ HH.text "Tunebank" ]
        , HH.text " web service for storing and transcoding its tunes."
        ]
    , HH.p_
        [ HH.text "Code is written in PureScript and PureScript-Halogen. "
        , HH.text "The idea for the polska metronome was borrowed from Ben Potton of the band "
        , HH.a
            [ HP.href "http://www.jigfoot.com/" ]
            [ HH.text "JigFoot." ]
        , HH.text " The tune players create the melody by virtue of "
        , HH.a
            [ HP.href "https://github.com/gleitz/midi-js-soundfonts" ]
            [ HH.text "pre-rendered soundfonts" ]
        , HH.text " from Benjamin Gleitzman."
        ]
    ]
