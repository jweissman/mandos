module Weapon exposing (Weapon, damage, describe, averageDamage, dagger, sword, axe)

import Material exposing (Material)
import Util

import String

type Family = Sword
            | Axe
            | Dagger
            --| Rapier
            --| Mace

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

averageDamage : Weapon -> Int
averageDamage weapon =
  let
    dmgRange =
      (baseDamage weapon.family)

    midpoint =
      (List.length dmgRange) // 2

    avg = 
      Util.getAt dmgRange midpoint
      |> Maybe.withDefault 1
      |> toFloat
  in
     round (avg * (Material.strength weapon.material))

damage : Int -> Int -> Weapon -> Int
damage m n {family,material} =
  let
    multiplier =
      (Material.strength material)

    damage =
      Util.sample m n 1 (baseDamage family)
      |> toFloat
  in
    round (damage * multiplier)

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

baseDamage : Family -> List Int
baseDamage family =
  case family of
    Sword -> [2..8]
    Dagger -> [1..4]
    Axe -> [3..5]
