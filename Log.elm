module Log exposing (Model, init, view)

import Creature
import Event exposing (Event)

import Html
import Graphics
import Svg 

-- MODEL
type alias Model = List Event

-- INIT
init : Model
init = [ Event.awaken ]

origin = (1,35)

-- VIEW
view : Model -> List (Svg.Svg a)
view model =
  let
    notes =
      List.take 5 (List.reverse (List.map Event.describe model))

    logLines =
      List.indexedMap logLineView notes

    header =
      Graphics.render "EVENTS" origin "grey"

  in
    [header] ++ logLines

logLineView : Int -> String -> Svg.Svg a
logLineView idx note =
  let (ox,oy) = origin in
  Graphics.render note (ox,oy+1+idx) ("#" ++ (toString (7-idx)) ++ "808" ++ (toString (7-idx)) ++ "8")
