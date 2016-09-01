module Action exposing (Action(..), describe, question, defaultForItem, canPerform, drop)

import Item exposing (Item, ItemKind(..))

type Action = Drop
            | Throw
            | Identify
            | Wield
            | Wear
            | Drink
            | Look

drop =
  Drop

throw =
  Throw

identify =
  Identify

wield =
  Wield

wear =
  Wear

drink =
  Drink

defaultForItem : Item -> Action
defaultForItem {kind} =
  case kind of
    Arm _ -> Wield
    Shield _ -> Wear
    Bottle _ -> Drink
    --_ -> Action.look

canPerform : Item -> Action -> Bool
canPerform {kind} action =
  True

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
