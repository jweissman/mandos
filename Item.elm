module Item exposing (Item, init, glyph, describe, ItemKind(..), weapon, armor, ring, helm, bottle, scroll, crystal, enchant, simple, canApply)

import Point exposing (Point)
import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Ring exposing (Ring)
import Helm exposing (Helm)
import Liquid exposing (Liquid)
import Spell exposing (Spell)

type QuestItemKind = Crystal

type ItemKind = Arm Weapon
              | Shield Armor
              | Bottle Liquid
              | Scroll Spell
              | Jewelry Ring
              | Headgear Helm
              | QuestItem QuestItemKind

weapon weapon' =
  Arm weapon'

armor armor' =
  Shield armor'

bottle liquid =
  Bottle liquid

scroll spell =
  Scroll spell

ring ring' =
  Jewelry ring'

helm helm' =
  Headgear helm'

crystal =
  QuestItem Crystal

type alias Item = { position : Point
                  , kind : ItemKind
                  , id : Int
                  }

init pt kind id =
  { position = pt
  , kind = kind
  , id = id
  }

simple kind =
  { position = (0,0), kind = kind, id = -101 }

glyph : Item -> String
glyph {kind} =
  case kind of
    Arm _ ->
      "!"

    Shield _ ->
      "%"

    Headgear _ ->
      "^"

    Jewelry _ ->
      "~"

    Bottle _ ->
      "?"

    Scroll _ ->
      "ø"

    QuestItem _ ->
      "∆"

describe : Item -> String
describe {kind} =
  case kind of
    Arm weapon' ->
      Weapon.describe weapon'

    Shield armor' ->
      Armor.describe armor'

    Jewelry ring ->
      Ring.describe ring

    Headgear helm ->
      Helm.describe helm

    Bottle liquid ->
      "bottle of " ++ (Liquid.describe liquid)

    Scroll spell ->
      "scroll of " ++ (Spell.describe spell)

    QuestItem kind ->
      case kind of
        Crystal ->
          "Crystal of Time"

enchant : Item -> Item
enchant item =
  case item.kind of
    Arm weapon ->
      { item | kind = Arm (Weapon.enchant weapon) }

    Shield armor ->
      { item | kind = Shield (Armor.enchant armor) }

    Jewelry ring ->
      { item | kind = Jewelry (Ring.enchant ring) }

    Headgear helm ->
      { item | kind = Headgear (Helm.enchant helm) }

    Bottle _ ->
      item

    Scroll _ ->
      item

    QuestItem _ ->
      item

canApply item' item =
  case item'.kind of
    Scroll spell ->
      if spell == Spell.infuse then
        case item.kind of
          Arm _ ->
            True

          Shield _ ->
            True

          _ ->
            False
      else
        False

    _ ->
      False
