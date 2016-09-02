module Weapon exposing (Weapon, damage, describe, dagger, sword, axe)

import Material exposing (Material)
import String

type Family = Sword
            | Axe
            | Dagger

type alias Weapon =
  { family : Family
  , material : Material
  }

axe : Material -> Weapon
axe material =
  { family = Axe
  , material = material
  }

dagger : Material -> Weapon
dagger material =
  { family  = Dagger
  , material = material
  }

sword : Material -> Weapon
sword material =
  { family  = Sword
  , material = material
  }

damage : Weapon -> Int
damage {family,material} =
  let multiplier = (Material.strength material) in
  round (baseDamage family * multiplier)

describe : Weapon -> String
describe {family,material} =
  [Material.describe material, describeFamily family ]
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
    Sword -> 3
    Dagger -> 2
    Axe -> 4
