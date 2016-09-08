module Entity exposing (Entity(..), view, describe, position, wall, floor, coin, player, monster, door, upstairs, downstairs, memory, entrance, crystal, imaginary, isCreature, item, grass)

import Point exposing (Point)
import Item
import Creature
import Warrior
import String
import Graphics
import Palette

import Svg

-- types
type Entity = Monster Creature.Model
            | Player Warrior.Model
            | Wall Point
            | Coin Point
            | Floor Point
            | Door Point
            | StairsUp Point
            | StairsDown Point
            | Memory Entity
            | Entrance Bool Point
            | Imaginary Entity
            | Item Item.Item
            | Grass Point

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

crystal pt =
  Item (Item.init pt Item.crystal -1)

entrance open pt =
  Entrance open pt

imaginary entity =
  Imaginary entity

item item' =
  Item item'

grass pt =
  Grass pt

isCreature entity =
  case entity of
    Monster _ ->
      True

    _ ->
      False

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

    Grass _ ->
      "a patch of grass"

    StairsUp _ ->
      "an upward-curving staircase"

    StairsDown _ ->
      "a downward-curving staircase"

    Memory entity ->
      "You saw " ++ (describe entity) ++ " here"

    Imaginary entity ->
      "You imagine there is " ++ (describe entity) ++ " here"

    Entrance open _ ->
      if open then
        "an open heavy metal gateway and daylight beyond"
      else
        "a closed heavy metal gateway"

    Item item ->
      Item.describe item

-- view
view : Entity -> Svg.Svg a
view entity =
  Graphics.render (glyph entity) (position entity) (color entity)

color : Entity -> String
color entity =
  case entity of
    Monster _ ->
      --"grey"
      Palette.secondary

    Player _ ->
      Palette.primaryLighter
      --"white"

    Wall _ ->
      Palette.tertiaryLight
      --"darkgrey"

    Coin _ ->
      Palette.secondaryLight

    Floor _ ->
      Palette.tertiaryLight
      --"rgba(160,160,240,0.5)"

    Door _ ->
      Palette.tertiaryLight
      --"orange"

    StairsUp _ ->
      Palette.tertiaryLight
      --"lightgray"

    StairsDown _ ->
      Palette.tertiaryLight
      --"lightgray"

    Memory _ ->
      Palette.primary' 3 0.8
      --"rgba(80,80,120,0.4)"

    Imaginary _ ->
      Palette.accent' 1 0.4
      --"green"

    Entrance open _ ->
      Palette.accentLight
      --if open then "green" else "red"

    Item _ ->
      Palette.secondaryLight
      --"yellow"

    Grass _ ->
      Palette.accentLight
      --"green"

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

    Entrance _ pt ->
      pt

    Memory entity ->
      position entity

    Imaginary entity ->
      position entity

    Item item ->
      item.position

    Grass pt ->
      pt

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

    Imaginary e ->
      glyph e

    Entrance _ _ ->
      "="

    Grass _ ->
      "\""

    Item item ->
      Item.glyph item
