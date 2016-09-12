module Graphics exposing (render, hero, jumbo)

import Point
import Palette
import Configuration

import Html
import Svg exposing (text')
import Svg.Attributes exposing (x, y, fontSize, fontFamily, fill, textAnchor, dominantBaseline)

font = "VT323"

render : String -> Point.Point -> String -> Svg.Svg a
render string (px,py) color =
  text' [ x (toString px)
        , y (toString py)
        , fontSize "1"
        , fontFamily font 
        , fill color
        ] [ Html.text string ]

render' : String -> Point.Point -> String -> String -> Svg.Svg a
render' string (px,py) color anchor =
  text' [ x (toString px)
        , y (toString py)
        , fontSize "1"
        , fontFamily font 
        , fill color
        , textAnchor anchor
        ] [ Html.text string ]

verticalCenter =
  Configuration.viewWidth//2

horizontalCenter =
  Configuration.viewHeight//2

hero : String -> Int -> Svg.Svg a
hero string py = -- (px,py) =
  text' [ x (toString verticalCenter)
        , y (toString py)
        , fontSize "16"
        , fontFamily font 
        , fill Palette.bright
        , textAnchor "middle"
        ] [ Html.text string ]

jumbo : String -> Svg.Svg a
jumbo string = --(px,py) =
  text' [ x (toString verticalCenter) --(Configuration.viewWidth//2))
        , y (toString (horizontalCenter//4))
        , fontSize "60"
        , fontFamily font
        , fill (Palette.primary' 2 0.4)
        , textAnchor "middle"
        , dominantBaseline "hanging"
        ] [ Html.text string ]
