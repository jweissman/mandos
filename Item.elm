module Item exposing (Item, init, glyph, describe, ItemKind(..), weapon, armor, bottle, scroll, crystal, enchant, simple)

import Point exposing (Point)
import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Liquid exposing (Liquid)
import Spell exposing (Spell)

type QuestItemKind = Crystal

type ItemKind = Arm Weapon
              | Shield Armor
              | Bottle Liquid
              | Scroll Spell
              --| Crystal
              | QuestItem QuestItemKind -- rtring

weapon weapon' =
  Arm weapon'

armor armor' =
  Shield armor'

bottle liquid =
  Bottle liquid

scroll spell =
  Scroll spell

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

-- simple ctor for cases where id/point aren't really relevant...?
-- where we need an item to wrap a weapon/armor keep everything 'the same' or close enough
simple kind =
  { position = (0,0), kind = kind, id = -101 }

glyph : Item -> String
glyph {kind} =
  case kind of
    Arm _ ->
      "!"

    Shield _ ->
      "%"

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
      Debug.log "ENCHANT WEAPON"
      { item | kind = Arm (Weapon.enchant weapon) }

    Shield armor ->
      Debug.log "ENCHANT ARMOR"
      { item | kind = Shield (Armor.enchant armor) }

    Bottle _ -> item
    Scroll _ -> item
    QuestItem _ -> item
