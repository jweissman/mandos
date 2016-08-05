module Warrior exposing (Model, init, view, step, takeDamage, enrich)

--import Geometry exposing (Direction, Point, slide, aDirection)

import Direction exposing (Direction(..))
import Point exposing (Point, slide)

import Svg exposing (text')
import Svg.Attributes exposing (x, y, fontSize, fontFamily)

import Html exposing (Html)

-- MODEL

type alias Model =
  { hp : Int
  , maxHp : Int
  , direction : Direction
  , position : Point
  , gold : Int
  , attack : Int
  , defense : Int
  , steps : Int
  }


-- INIT

init : Model
init =
  { hp = 10
  , maxHp = 10
  , direction = North
  , position = {x=5, y=5}
  , gold = 0
  , attack = 3
  , defense = 1
  , steps = 0
  }

-- UPDATE

-- helpers

step : Direction -> Model -> Model
step direction model =
  { model | position = slide model.position direction
          , steps = model.steps + 1
          }

takeDamage : Int -> Model -> Model
takeDamage amount model =
  { model | hp = model.hp - amount }

enrich : Int -> Model -> Model
enrich amount model =
  { model | gold = model.gold + amount }

-- VIEW
view : Model -> Svg.Svg a
view model =
  let
    x' = toString model.position.x
    y' = toString model.position.y
  in
    text' [ x x', y y', fontSize "1", fontFamily "Courier" ] [ Html.text "@" ]
