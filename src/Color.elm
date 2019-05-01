module Color exposing (base, darkest, gray, green, highlight, lightGray, lightGreen, white)

import Element exposing (Color, rgb255)


base : Color
base =
    rgb255 102 99 255


darkest : Color
darkest =
    rgb255 54 52 179


gray : Color
gray =
    rgb255 139 121 153


lightGray : Color
lightGray =
    rgb255 151 131 166


green : Color
green =
    rgb255 86 179 61


lightGreen : Color
lightGreen =
    rgb255 90 204 59


highlight : Color
highlight =
    rgb255 255 170 125


white : Color
white =
    rgb255 255 255 255
