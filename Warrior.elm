module Warrior exposing (Model, init, step, takeDamage, enrich, collectsItem, computeDamageAgainst, cardView)

import Direction exposing (Direction(..))
import Point exposing (Point, slide)

import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Item exposing (Item(..), ItemKind(..))

import Graphics
import Svg

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
  , armor : Maybe Armor
  }


-- INIT

init : Point -> Model
init point =
  { hp = 20
  , maxHp = 20
  , direction = North
  , position = point
  , gold = 0
  , attack = 1
  , defense = 0
  , steps = 0
  , weapon = Nothing
  , armor = Nothing
  }

power : Model -> Int
power model =
  case model.weapon of 
    Nothing -> 
      model.attack
    Just weapon ->
      model.attack + (Weapon.damage weapon)

resistance : Model -> Int
resistance model =
  case model.armor of
    Nothing ->
      model.defense
    Just armor ->
      model.defense + (Armor.absorption armor)

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
  max 1 ((power model) - defense)

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
    Shield armor ->
      { model | armor = Just armor }


---

cardView : Point -> Model -> List (Svg.Svg a)
cardView (x,y) model =
  let
    wielding =
      case model.weapon of
        Nothing -> "(none)"
        Just weapon ->
          Weapon.describe weapon

    wearing =
      case model.armor of
        Nothing -> "(none)"
        Just armor ->
          Armor.describe armor

    strength =
      toString (power model)

    resist =
      toString (resistance model)
  in
    [ Graphics.render "EQUIPMENT" (x, y) "darkgray"
    , Graphics.render ("WEAPON: " ++ wielding) (x, y+2) "lightgray"
    , Graphics.render (" ARMOR: " ++ wearing) (x, y+3) "lightgray"
    , Graphics.render "STATS" (x, y+7) "darkgray"
    , Graphics.render ("  STRENGTH: " ++ strength) (x, y+9) "lightgray"
    , Graphics.render ("RESISTANCE: " ++ resist) (x, y+10) "lightgray"
    ]
