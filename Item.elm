module Item exposing (Item, init, glyph, name, describe, ItemKind(..), weapon, armor, ring, helm, bottle, scroll, crystal, enchant, simple, canApply)

import Point exposing (Point)
import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Ring exposing (Ring)
import Helm exposing (Helm)
import Liquid exposing (Liquid)
import Spell exposing (Spell)
import Language exposing (Language)
import Idea

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

--kind item =

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

name : Item -> String
name item =
  case item.kind of
    Arm weapon' ->
      Weapon.describe weapon'

    Shield armor' ->
      Armor.describe armor'

    Jewelry ring ->
      Idea.describe (Spell.idea (Ring.spell ring)) -- vocab language ring

    Headgear helm ->
      Helm.describe helm

    Bottle liquid ->
      "bottle of "
      ++ (Idea.describe (Liquid.idea liquid))

    Scroll spell ->
      "scroll of "
      ++ (Idea.describe (Spell.idea spell))

    QuestItem kind ->
      case kind of
        Crystal ->
          "Crystal of Time"

describe : Language -> Language -> Item -> String
describe vocab language {kind} =
  case kind of
    Arm weapon' ->
      Weapon.describe weapon'

    Shield armor' ->
      Armor.describe armor'

    Jewelry ring ->
      Ring.describe vocab language ring

    Headgear helm ->
      Helm.describe helm

    Bottle liquid ->
      "bottle of " --++ (Liquid.describe liquid)
      ++ (Language.decode (Liquid.idea liquid) vocab language)

    Scroll spell ->
      "scroll of " --++ (Spell.describe spell)
      ++ (Language.decode (Spell.idea spell) vocab language)

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

          Jewelry _ ->
            True

          Headgear _ ->
            True

          _ ->
            False
      else
        False

    _ ->
      False

