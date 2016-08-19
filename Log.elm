module Log exposing (Model, init, view)

import Creature
import Event exposing (Event)

import Html
import Graphics
import Svg 
--exposing (text')
--import Svg.Attributes exposing (x,y,fontSize,fontFamily)

-- MODEL
type alias Model = List Event

-- INIT
init : Model
init = [ Event.awaken ]

origin = {x = 1, y = 25}

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
  Graphics.render note { origin | y=origin.y+1+idx } ("#" ++ (toString (7-idx)) ++ "808" ++ (toString (7-idx)) ++ "8")
  --text' [ x "15", y (toString (3+idx)), fontSize "1", fontFamily "Courier" ] [ Html.text note ]
