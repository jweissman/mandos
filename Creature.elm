module Creature exposing (Model, view, step, turn, describe, createRat, createMonkey, createBandit)

--import Geometry exposing (Point, Direction, slide)

import Species exposing (Species)

import Point exposing (Point, slide)
import Direction exposing (Direction(..))

import Svg exposing (text')
import Svg.Attributes exposing (x,y,fontSize,fontFamily)

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
  , direction : Direction
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
  , defense = 1
  , attack = 2
  , direction = North
  }

createRat id point =
  init Species.rat id point

createMonkey id point =
  init Species.monkey id point

createBandit id point =
  init Species.bandit id point

step model =
  let
    position =
      slide model.position model.direction
  in
    { model | position = position }

turn direction model =
  { model | direction = direction }

describe : Model -> String
describe model =
  model.name ++ " (" ++ toString model.hp ++ "/" ++ toString model.maxHp ++ ")"

-- VIEW
view : Model -> Svg.Svg a
view model =
  text' [ x (toString model.position.x), y (toString model.position.y), fontSize "1", fontFamily "Courier" ] [ Html.text (String.fromChar model.glyph) ]
