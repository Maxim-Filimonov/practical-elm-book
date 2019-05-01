module Attr exposing (defaultBorders, error, grayButton, greenButton, input)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


defaultBorders =
    { bottom = 0
    , top = 0
    , left = 0
    , right = 0
    }


greenButton : List (Attribute msg)
greenButton =
    [ Background.color green
    , Border.color lightGreen
    , Border.rounded 3
    , Border.widthEach { defaultBorders | bottom = 3 }
    , Font.bold
    , Font.color white
    , paddingXY 20 6
    ]


grayButton : List (Attribute msg)
grayButton =
    [ Background.color gray
    , Border.color lightGray
    , Border.rounded 3
    , Border.widthEach { defaultBorders | bottom = 1, right = 1 }
    , Font.color white
    ]


input : List (Attribute msg)
input =
    []


error : List (Attribute msg)
error =
    [ Font.color highlight
    ]
