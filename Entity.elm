module Entity exposing (Entity(..), view, describe, position, wall, floor, coin, player, monster, door, upstairs, downstairs, memory, entrance, crystal, imaginary, isCreature)


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
            | Entrance Bool Point
            | Crystal Bool Point
            | Imaginary Entity

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

crystal taken pt =
  Crystal taken pt

entrance open pt =
  Entrance open pt

imaginary entity =
  Imaginary entity

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

    StairsUp _ ->
      "an upward-curving staircase"

    StairsDown _ ->
      "a downward-curving staircase"

    Memory entity ->
      "You saw " ++ (describe entity) ++ " here"

    Imaginary entity ->
      "You imagine there might be " ++ (describe entity) ++ " here"
      
    Crystal taken _ ->
      if taken then
        "a pedestal where the Crystal was"
      else
        "the shimmering Crystal"

    Entrance open _ ->
      if open then
        "an open heavy metal gateway and daylight beyond"
      else
        "a closed heavy metal gateway"

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
      "rgba(80,80,120,0.4)"

    Imaginary _ ->
      "green"

    Crystal taken _ ->
      if taken then "gray" else "white"

    Entrance open _ ->
      if open then "green" else "red"

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

    Crystal _ pt ->
      pt

    Entrance _ pt ->
      pt

    Memory entity ->
      position entity

    Imaginary entity ->
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

    Imaginary e ->
      glyph e

    Entrance _ _ ->
      "∞" 

    Crystal _ _ ->
      "∆"
