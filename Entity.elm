module Entity exposing (Entity(..), view, describe, position, wall, floor, coin, player, monster, door, upstairs, downstairs, memory)


import Point exposing (Point)
import Creature
import Warrior

import String
import Graphics
import Svg

-- types

type Orientation = Up | Down

type Accessible = Open | Closed 

type Entity = Monster Creature.Model
            | Player Warrior.Model
            | Wall Point
            | Coin Point
            | Floor Point
            | Door Point
            | StairsUp Point
            | StairsDown Point
            | Memory Entity
            --| Entrance Bool Point
            --| Crystal Bool Point

-- constructors
wall point =
  Wall point

coin point =
  Coin point

floor point =
  Floor point

door point =
  Door point

player warrior =
  Player warrior

monster creature =
  Monster creature

upstairs point =
  StairsUp point

downstairs point =
  StairsDown point

memory entity =
  Memory entity

-- helpers

describe : Entity -> String
describe entity =
  case entity of
    Monster creature ->
      Creature.describe creature

    Player player ->
      "a nameless warrior"

    Wall _ ->
      "a sturdy wall"

    Coin _ ->
      "a golden coin"

    Floor _ ->
      "a cobblestone floor"

    Door _ ->
      "a creaky door"

    StairsUp _ ->
      "an upward-curving staircase"

    StairsDown _ ->
      "a downward-curving staircase"

    Memory entity ->
      "You saw " ++ (describe entity) ++ " here"

-- view
view : Entity -> Svg.Svg a
view entity =
  Graphics.render (glyph entity) (position entity) (color entity) --"darkgreen"

color : Entity -> String
color entity =
  case entity of
    Monster _ ->
      "grey"

    Player _ ->
      "white"

    Wall _ ->
      "grey"

    Coin _ ->
      "gold"

    Floor _ ->
      "gray"

    Door _ ->
      "orange"

    StairsUp _ ->
      "green"

    StairsDown _ ->
      "yellow"

    Memory _ ->
      "darkblue"

position : Entity -> Point.Point
position entity =
  case entity of
    Monster creature ->
      creature.position

    Player player ->
      player.position

    Door point ->
      point

    Wall point ->
      point

    Coin point ->
      point

    Floor point ->
      point

    StairsUp point ->
      point

    StairsDown point ->
      point

    Memory entity ->
      position entity

glyph : Entity -> String
glyph entity =
  case entity of
    Monster creature ->
      String.fromChar creature.glyph

    Player _ ->
      "@"

    Wall _ ->
      "#"

    Coin _ ->
      "*"

    Floor _ ->
      "."

    Door _ ->
      "+"

    StairsUp _ ->
      ">"

    StairsDown _ ->
      "<"
    
    Memory e ->
      glyph e

