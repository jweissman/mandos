module Log exposing (Model, init, view)

import Creature
import Event exposing (Event)

import Html
import Svg exposing (text')
import Svg.Attributes exposing (x,y,fontSize,fontFamily)

-- MODEL
type alias Model = List Event

-- INIT
init : Model
init = [ Event.awakenEvent ]

-- VIEW
view : Model -> List (Svg.Svg a)
view model =
  let
    notes =
      List.take 5 (List.reverse (List.map Event.describeEvent model))

    logLines =
      List.indexedMap logLineView notes

    header =
      text' [ x "15", y "2", fontSize "1", fontFamily "Courier" ] [ Html.text "EVENT LOG:" ]

  in
    [header] ++ logLines

logLineView : Int -> String -> Svg.Svg a
logLineView idx note =
  text' [ x "15", y (toString (3+idx)), fontSize "1", fontFamily "Courier" ] [ Html.text note ]
