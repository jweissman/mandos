module Item exposing (Item(..), init, position, glyph, describe, ItemKind(..), weapon, armor)

import Point exposing (Point)
import Weapon exposing (Weapon)
import Armor exposing (Armor)

type ItemKind = Arm Weapon
              | Shield Armor

type Item = Item Point ItemKind

weapon weapon' =
  Arm weapon'

armor armor' =
  Shield armor'

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

    Shield _ ->
      "%"

describe : Item -> String
describe (Item _ kind) =
  case kind of
    Arm weapon' ->
      Weapon.describe weapon'

    Shield armor' ->
      Armor.describe armor'
