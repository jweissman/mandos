module Weapon exposing (Weapon, woodenSword, damage, describe)

type Material = Iron
              | Wood
              | Steel

type Family = Sword
            | Axe

type alias Weapon =
  { family : Family
  , material : Material
  }

woodenSword : Weapon
woodenSword =
  { family   = Sword
  , material = Wood
  }

damage : Weapon -> Int
damage weapon =
  round ((baseDamage weapon.family) * (materialMultiplier weapon.material))

describe : Weapon -> String
describe weapon =
  case weapon.family of
    Sword -> 
      "a sword"

    Axe ->
      "an axe"

baseDamage : Family -> Float
baseDamage family =
  case family of
    Sword -> 3
    Axe -> 4

materialMultiplier : Material -> Float
materialMultiplier material =
  case material of
    Wood  -> 0.3
    Iron  -> 1.0
    Steel -> 1.75
