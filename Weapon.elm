module Weapon exposing (Weapon, woodenSword, damage, describe)

import Material exposing (Material)
import String

type Family = Sword
            | Axe

type alias Weapon =
  { family : Family
  , material : Material
  }

woodenSword : Weapon
woodenSword =
  { family   = Sword
  , material = Material.wood
  }

damage : Weapon -> Int
damage {family,material} =
  let multiplier = (Material.strength material) in
  round (baseDamage family * multiplier)

describe : Weapon -> String
describe {family,material} =
  ["a", Material.describe material, describeFamily family ] 
  |> String.join " "

describeFamily : Family -> String
describeFamily family =
  case family of
    Sword -> 
      "sword"

    Axe ->
      "axe"

baseDamage : Family -> Float
baseDamage family =
  case family of
    Sword -> 7
    Axe -> 10
