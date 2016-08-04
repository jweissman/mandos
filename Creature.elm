module Creature exposing (Model, view, step, describe, createRat, createMonkey)

import Geometry exposing (Point, Direction, slide)

import Svg exposing (text')
import Svg.Attributes exposing (x,y,fontSize,fontFamily)

import Html
import String

-- MODEL

type Species = Bandit | Rat | Snake | Tiger | Dragon | Monkey

speciesGlyph : Species -> Char
speciesGlyph species =
  case species of
    Bandit -> 'b'
    Rat    -> 'r'
    Snake  -> 's'
    Tiger  -> 't'
    Dragon -> 'd'
    Monkey -> 'm'

speciesName : Species -> String
speciesName species =
  case species of
    Bandit -> "bandit"
    Rat    -> "rat"
    Snake  -> "snake"
    Tiger  -> "tiger"
    Dragon -> "drake"
    Monkey -> "monkey"

speciesHp : Species -> Int
speciesHp species =
  case species of
    Rat -> 10
    Snake -> 20
    Monkey -> 35
    Bandit -> 50
    Tiger -> 100
    Dragon -> 200

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
  }

-- INIT

init : Species -> Int -> Point -> Model
init species id point =
  { id = id
  , hp = speciesHp species
  , maxHp = speciesHp species
  , position = point
  , species = species
  , glyph = speciesGlyph species
  , name = speciesName species
  , defense = 2
  , attack = 3
  }

createRat id point =
  init Rat id point

createMonkey id point =
  init Monkey id point

step direction model =
  { model | position = slide model.position direction }

describe : Model -> String
describe model =
  model.name ++ " (" ++ toString model.hp ++ "/" ++ toString model.maxHp ++ ")"

-- VIEW
view : Model -> Svg.Svg a
view model =
  text' [ x (toString model.position.x), y (toString model.position.y), fontSize "1", fontFamily "Courier" ] [ Html.text (String.fromChar model.glyph) ]
