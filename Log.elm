module Log exposing (Model, init, view)

import Creature
import Event exposing (Event)
import Point exposing (Point)
import Graphics

import Svg 

-- MODEL
type alias Model = List Event

-- INIT
init : Model
init = [ Event.awaken ]

maxEntries = 7

-- VIEW
view : Point -> Model -> List (Svg.Svg a)
view origin model =
  let
    notes =
      List.take maxEntries (List.reverse (List.map Event.describe model))

    logLines =
      List.indexedMap (logLineView origin) notes

    header =
      Graphics.render "EVENTS" origin "grey"

  in
    [header] ++ logLines

logLineView : Point -> Int -> String -> Svg.Svg a
logLineView origin idx note =
  let (ox,oy) = origin in
  Graphics.render note (ox,oy+1+idx) ("#" ++ (toString (9-idx)) ++ "808" ++ (toString (9-idx)) ++ "8")
