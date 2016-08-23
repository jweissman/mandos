module Entity exposing (Entity, view, describe, position, wall, floor, coin, player, monster, door, upstairs, downstairs)


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

-- view
view : Entity -> Svg.Svg a
view entity =
  Graphics.render (glyph entity) (position entity) (color entity) --"darkgreen"

color : Entity -> String
color entity =
  case entity of
    Monster _ ->
      "darkgrey"

    Player _ ->
      "white"

    Wall _ ->
      "darkgrey"

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

