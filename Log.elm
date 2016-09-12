module Log exposing (Model, init, view)

import Creature
import Event exposing (Event)
import Point exposing (Point)
import Graphics
import Palette
import Language exposing (Language)

import Svg

-- MODEL
type alias Model = List Event

-- INIT
init : Model
init = [ Event.awaken ]

maxEntries = 7

-- VIEW
view : Point -> Language -> Language -> List Event -> List (Svg.Svg a)
view origin vocab lang model =
  let
    notes =
      List.take maxEntries (List.reverse (List.map (Event.describe vocab lang) model))

    logLines =
      List.indexedMap (logLineView origin) notes

    header =
      Graphics.render "EVENTS" origin "white"

  in
    [header] ++ logLines

logLineView : Point -> Int -> String -> Svg.Svg a
logLineView origin idx note =
  let (ox,oy) = origin in
  Graphics.render note (ox,oy+1+idx) (Palette.rgb (Palette.blue idx))
