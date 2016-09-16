module Item exposing (Item, init, glyph, name, describe, ItemKind(..), weapon, armor, ring, helm, bottle, scroll, javelin, crystal, enchant, simple, canApply, thrownDamage)

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

type AmmoKind = Spear
              | Javelin

type ItemKind = Arm Weapon
              | Throwing AmmoKind
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

javelin =
  Throwing Javelin

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
      ")"

    Shield _ ->
      "["

    Headgear _ ->
      "^"

    Jewelry _ ->
      "&"

    Bottle _ ->
      "?"

    Scroll _ ->
      "~"

    QuestItem _ ->
      "∆"

    Throwing _ ->
      "|"

name : Item -> String
name item =
  case item.kind of
    Arm weapon' ->
      Weapon.describe weapon'

    Shield armor' ->
      Armor.describe armor'

    Jewelry ring ->
      "ring of "
      ++ (Idea.describe (Spell.idea (Ring.spell ring)))

    Headgear helm ->
      Helm.describe helm

    Bottle liquid ->
      "bottle of "
      ++ (Idea.describe (Liquid.idea liquid))

    Scroll spell ->
      "scroll of "
      ++ (Idea.describe (Spell.idea spell))

    Throwing thrown ->
      case thrown of
        Spear -> "spear"
        Javelin -> "javelin"

    QuestItem kind ->
      case kind of
        Crystal ->
          "crystal of time"

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
      "bottle of "
      ++ (Language.decode (Liquid.idea liquid) vocab language)

    Scroll spell ->
      "scroll of "
      ++ (Language.decode (Spell.idea spell) vocab language)

    Throwing thrown ->
      case thrown of
        Spear -> "spear"
        Javelin -> "javelin"

    QuestItem kind ->
      case kind of
        Crystal ->
          "crystal of time"

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

    Throwing thrown ->
      item

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


thrownDamage item =
  case item.kind of
    Throwing ammo ->
      200

    _ ->
      1

