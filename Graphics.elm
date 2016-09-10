module Graphics exposing (render, hero, jumbo)

import Point
import Palette

import Html

import Svg exposing (text')
import Svg.Attributes exposing (x, y, fontSize, fontFamily, fill)

font = "VT323"

render : String -> Point.Point -> String -> Svg.Svg a
render string (px,py) color =
  text' [ x (toString px)
        , y (toString py)
        , fontSize "1"
        , fontFamily font 
        , fill color
        ] [ Html.text string ]

hero : String -> Point.Point -> Svg.Svg a
hero string (px,py) =
  text' [ x (toString px)
        , y (toString py)
        , fontSize "16"
        , fontFamily font 
        , fill Palette.bright
        ] [ Html.text string ]

jumbo : String -> Point.Point -> Svg.Svg a
jumbo string (px,py) =
  text' [ x (toString px)
        , y (toString py)
        , fontSize "60"
        , fontFamily font
        , fill (Palette.primary' 1 0.2)
        ] [ Html.text string ]


