module Item exposing (Item(..), init, position, glyph, describe, ItemKind(..), arm)

import Point exposing (Point)
import Weapon exposing (Weapon)

type ItemKind = Arm Weapon
type Item = Item Point ItemKind

arm weapon =
  Arm weapon

init pt kind =
  Item pt kind

position : Item -> Point
position (Item pt _) = 
  pt

glyph : Item -> String
glyph (Item _ kind) =
  case kind of
    Arm _ -> 
      "!"

describe : Item -> String
describe (Item _ kind) =
  case kind of
    Arm weapon ->
      Weapon.describe weapon
