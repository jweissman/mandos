module Weapon exposing (Weapon, damage, describe, ironDagger, ironSword)

import Material exposing (Material)
import String

type Family = Sword
            | Axe
            | Dagger

type alias Weapon =
  { family : Family
  , material : Material
  }

ironSword : Weapon
ironSword =
  { family   = Sword
  , material = Material.iron
  }

ironDagger : Weapon
ironDagger =
  { family  = Sword
  , material = Material.iron
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

    Dagger ->
      "dagger"

baseDamage : Family -> Float
baseDamage family =
  case family of
    Sword -> 5
    Dagger -> 4
    Axe -> 3
