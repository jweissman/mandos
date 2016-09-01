module Item exposing (Item, init, glyph, describe, ItemKind(..), weapon, armor)

import Point exposing (Point)
import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Liquid exposing (Liquid)

type ItemKind = Arm Weapon
              | Shield Armor
              | Bottle Liquid

weapon weapon' =
  Arm weapon'

armor armor' =
  Shield armor'

bottle liquid =
  Bottle liquid

type alias Item = { position : Point
                  , kind : ItemKind
                  , id : Int
                  }

init pt kind id =
  { position = pt
  , kind = kind
  , id = id
  }

glyph : Item -> String
glyph {kind} =
  case kind of
    Arm _ ->
      "!"

    Shield _ ->
      "%"

    Bottle _ ->
      "?"

describe : Item -> String
describe {kind} =
  case kind of
    Arm weapon' ->
      Weapon.describe weapon'

    Shield armor' ->
      Armor.describe armor'

    Bottle liquid ->
      "Bottle of " ++ (Liquid.describe liquid)
