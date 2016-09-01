module Armor exposing (Armor, absorption, describe, leatherTunic, leatherSuit)

import Material exposing (Material)

import String

type Kind = Suit
          | Tunic

type alias Armor =
  { kind : Kind
  , material : Material
  }

leatherTunic : Armor
leatherTunic =
  { kind  = Tunic
  , material = Material.leather
  }

leatherSuit : Armor
leatherSuit =
  { kind  = Suit
  , material = Material.leather
  }

absorption : Armor -> Int
absorption {kind, material} =
  let mult = (Material.resistance material) in
  round (mult * baseResist kind)

baseResist : Kind -> Float
baseResist family =
  case family of
    Tunic -> 4
    Suit -> 5

describe : Armor -> String
describe {kind,material} =
  ["a", Material.describe material, describeKind kind ]
  |> String.join " "

describeKind : Kind -> String
describeKind family =
  case family of
    Suit ->
      "suit"
    Tunic ->
      "tunic"
