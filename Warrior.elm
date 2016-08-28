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
  { hp = 20
  , maxHp = 20
  , direction = North
  , position = point
  , gold = 0
  , attack = 3
  , defense = 1
  , steps = 0
  }

-- UPDATE

-- helpers

step : Direction -> Model -> Model
step direction model =
  let model' = { model | position = model.position |> slide direction
          , steps = model.steps + 1
          }
  in
    if model.steps % 10 == 0 then
      model' |> heal 1
    else
      model'

takeDamage : Int -> Model -> Model
takeDamage amount model =
  { model | hp = model.hp - amount }

enrich : Int -> Model -> Model
enrich amount model =
  { model | gold = model.gold + amount }

heal : Int -> Model -> Model
heal amount model =
  { model | hp = min model.maxHp (model.hp + 1) }


-- VIEW
--view : Model -> Svg.Svg a
--view model =
  --Entity.render model
  --Graphics.render "@" model.position
