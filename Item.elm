module Item exposing (Item, init, glyph, describe, ItemKind(..), weapon, armor, bottle, scroll, crystal, enchant)

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
  --Debug.log "NEW ITEM"
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
      { item | kind = Arm (Weapon.enchant weapon) }

    Shield armor ->
      { item | kind = Shield (Armor.enchant armor) }

    Bottle _ -> item
    Scroll _ -> item
    QuestItem _ -> item
