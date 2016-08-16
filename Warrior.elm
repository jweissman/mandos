module Warrior exposing (Model, init, step, takeDamage, enrich)

import Direction exposing (Direction(..))
import Point exposing (Point, slide)

--import Entity

import Graphics
import Svg
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

init : Point -> Model
init point =
  { hp = 10
  , maxHp = 10
  , direction = North
  , position = point -- {x=5, y=5}
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
--view : Model -> Svg.Svg a
--view model =
  --Entity.render model
  --Graphics.render "@" model.position
