module Styles exposing (..)

import Style exposing (..)
import Style.Color as Color
import Color exposing (..)


type Styles
    = Entry
    | None


type Variations
    = Selected


stylesheet =
    Style.stylesheet
        [ style Entry
            [ Color.background blue
            , variation Selected
                [ Color.background gray
                ]
            ]
        , style None []
        ]
