module Warrior exposing (Model, init, step, takeDamage, enrich, collectsItem, computeDamageAgainst)

import Direction exposing (Direction(..))
import Point exposing (Point, slide)

import Weapon exposing (Weapon)
import Item exposing (Item(..), ItemKind(..))

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
  , weapon : Maybe Weapon
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
  , weapon = Nothing
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

computeDamageAgainst : Int -> Model -> Int
computeDamageAgainst defense model =
  case model.weapon of
    Nothing -> 
      max 1 (model.attack - defense)

    Just weapon ->
      max 1 (model.attack + (Weapon.damage weapon) - defense)

takeDamage : Int -> Model -> Model
takeDamage amount model =
  { model | hp = model.hp - amount }

enrich : Int -> Model -> Model
enrich amount model =
  { model | gold = model.gold + amount }

heal : Int -> Model -> Model
heal amount model =
  { model | hp = min model.maxHp (model.hp + 1) }

collectsItem : Item -> Model -> Model
collectsItem (Item _ kind) model =
  case kind of
    Arm weapon -> -- autoequip, no inv for now
      { model | weapon = Just weapon }
