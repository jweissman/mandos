module Graphics exposing (render, hero, jumbo)

import Point

import Html

import Svg exposing (text')
import Svg.Attributes exposing (x, y, fontSize, fontFamily, fill)

font = "VT323" --Source Code Pro"

render : String -> Point.Point -> String -> Svg.Svg a
render string (px,py) color =
  text' [ x (toString px)
        , y (toString py)
        , fontSize "1"
        , fontFamily font 
        , fill color --"darkgreen"
        ] [ Html.text string ]

hero : String -> Point.Point -> Svg.Svg a
hero string (px,py) =
  text' [ x (toString px)
        , y (toString py)
        , fontSize "9"
        , fontFamily font 
        , fill "white" --"darkgreen"
        ] [ Html.text string ]

jumbo : String -> Point.Point -> Svg.Svg a
jumbo string (px,py) =
  text' [ x (toString px)
        , y (toString py)
        , fontSize "35"
        , fontFamily font 
        , fill "rgba(192,192,192,0.3)" --"darkgreen"
        ] [ Html.text string ]


