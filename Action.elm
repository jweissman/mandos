module Action exposing (Action(..), describe, question, defaultForItem, canPerform, drop, enchant, use)

import Item exposing (Item, ItemKind(..))

type Action = Drop
            | Throw
            | Identify
            | Wield
            | Wear
            | Drink
            | Look
            | Read
            | Enchant
            | Use Item Action

drop =
  Drop

enchant =
  Enchant

use item action =
  Use item action

--throw =
--  Throw
--
--identify =
--  Identify
--
--wield =
--  Wield
--
--wear =
--  Wear
--
--drink =
--  Drink
--
--read =
--  Read

defaultForItem : Item -> Action
defaultForItem {kind} =
  case kind of
    Arm _ -> Wield
    Shield _ -> Wear
    Bottle _ -> Drink
    Scroll _ -> Read
    QuestItem _ -> Look

canPerform : Item -> Action -> Bool
canPerform item action =
  let {kind} = item in
  case action of
    Wield ->
      case kind of
        Arm _ -> True
        _ -> False

    Wear ->
      case kind of
        Shield _ -> True
        _ -> False

    Read ->
      case kind of
        Scroll _ -> True
        _ -> False

    Drink ->
      case kind of
        Bottle _ -> True
        _ -> False

    Drop ->
      case kind of
        QuestItem _ -> False
        _ -> True

    Enchant ->
      case kind of
        Arm _ -> True
        Shield _ -> True
        _ -> False

    Use item' action' ->
      canPerform item action'

    _ -> False

describe : Action -> String
describe action =
  case action of
    Drop ->
      "Remove"

    Throw ->
      "Hurl"

    Identify ->
      "Identify"

    Wield ->
      "Equip"

    Wear ->
      "Equip"

    Drink ->
      "Drink"

    Look ->
      "Search"

    Read ->
      "Read"

    Enchant ->
      "Enchant"

    Use item action' ->
      describe action'

question : Action -> String
question action =
  case action of
    Drop ->
      "What would you like to drop?"

    Throw ->
      "What would you like to throw?"

    Identify ->
      "What mysterious object would you like to identify?"

    Wield ->
      "Which weapon would you like to wield?"

    Wear ->
      "What would you like to wear?"

    Drink ->
      "What would you like to drink?"

    Look ->
      "Where would you like to look?"

    Read ->
      "What would you like to read?"

    Enchant ->
      "What would you like to enchant?"

    Use item action' ->
      "What would you like to " ++ (describe action') ++ " with " ++ (Item.describe item) ++ "?"
