module Styles exposing (..)

import Style exposing (..)
import Style.Color as Color
import Color exposing (..)


type Styles
    = Entry
    | Base
    | None


type Variations
    = Selected


baseColor =
    Color.hsl (degrees 200) 0.18 0.26


highlightColor =
    Color.hsl (degrees 200) 0.18 0.46


textColor =
    -- Color.hsl (degrees 208) 1 0.97
    Color.hsl (degrees 200) 1 0.97


stylesheet =
    Style.stylesheet
        [ style Entry
            [ Color.background baseColor
            , variation Selected
                [ Color.background highlightColor
                ]
            ]
        , style Base [ Color.background baseColor, Color.text textColor ]
        , style None []
        ]
