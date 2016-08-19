module Graphics exposing (render)

import Point

import Html

import Svg exposing (text')
import Svg.Attributes exposing (x, y, fontSize, fontFamily, fill)

font = "VT323" --Source Code Pro"

render : String -> Point.Point -> String -> Svg.Svg a
render string point color =
  text' [ x (toString point.x)
        , y (toString point.y)
        , fontSize "1"
        , fontFamily font 
        , fill color --"darkgreen"
        ] [ Html.text string ]
