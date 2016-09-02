module Creature exposing (Model, init, step, turn, injure, describe, engage, disengage)

import Species exposing (Species)

import Point exposing (Point, slide)
import Direction exposing (Direction(..))

import Svg
import Graphics

import Html
import String

-- MODEL
type alias Model =
  { id : Int
  , hp : Int
  , maxHp : Int
  , defense : Int
  , attack : Int
  , position : Point
  , species : Species
  , glyph : Char
  , name : String
  , subtype : String
  , direction : Direction
  , engaged : Bool
  }

-- INIT

init : Species -> Int -> Point -> Model
init species id point =
  { id = id
  , hp = Species.hp species
  , maxHp = Species.hp species
  , position = point
  , species = species
  , glyph = Species.glyph species
  , name = Species.name species
  , defense = Species.resistance species
  , attack = Species.power species
  , direction = North
  , engaged = False
  , subtype = Species.adjective species
  }

step : Model -> Model
step model =
  let
    position =
      model.position
      |> slide model.direction
  in
    { model | position = position }

turn : Direction -> Model -> Model
turn direction model =
  { model | direction = direction }

injure : Int -> Model -> Model
injure amount model =
  let hp' = model.hp - amount in
  { model | hp = max 0 hp' }

describe : Model -> String
describe model =
  let
    parts =
      [ "the", describeHealth model, model.subtype, model.name ]
  in
    parts
    |> String.join " "

describeHealth : Model -> String
describeHealth model =
  if model.hp == model.maxHp then
    "healthy"
  else
    if model.hp > (model.maxHp // 2) then
      "hurt"
    else
      "badly hurt"

engage : Model -> Model
engage model =
  { model | engaged = True }

disengage : Model -> Model
disengage model =
  { model | engaged = False }
