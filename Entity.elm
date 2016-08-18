module Entity exposing (Entity, view, describe, position, wall, floor, coin, player, monster)


import Point
import Creature
import Warrior

import String
import Graphics
import Svg

-- types

type Entity = Monster Creature.Model
            | Player Warrior.Model
            | Wall Point.Point
            | Coin Point.Point
            | Floor Point.Point

-- constructors

wall point =
  Wall point

coin point =
  Coin point

floor point =
  Floor point

player warrior =
  Player warrior

monster creature =
  Monster creature

-- helpers

describe : Entity -> String
describe entity =
  case entity of
    Monster creature ->
      Creature.describe creature

    Player player ->
      "a nameless warrior"

    Wall point ->
      "a sturdy wall"

    Coin point ->
      "a golden coin"

    Floor point ->
      "a cobblestone floor"

-- view
view : Entity -> Svg.Svg a
view entity =
  Graphics.render (glyph entity) (position entity) (color entity) --"darkgreen"

color : Entity -> String
color entity =
  case entity of
    Monster _ ->
      "lightgray"

    Player _ ->
      "white"

    Wall _ ->
      "darkgrey"

    Coin _ ->
      "gold"

    Floor _ ->
      "gray"

position : Entity -> Point.Point
position entity =
  case entity of
    Monster creature ->
      creature.position

    Player player ->
      player.position

    Wall point ->
      point

    Coin point ->
      point

    Floor point ->
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
      "."

    Floor _ ->
      " "
