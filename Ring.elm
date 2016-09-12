module Ring exposing (Ring, describe, enchant, light, power, strengthBonus, visionBonus)

import Spell exposing (Spell)

type Ring = Annulus Spell
          | Enchanted Int Ring

light : Ring
light =
  Annulus Spell.lux

power : Ring
power =
  Annulus Spell.infuse

describe : Ring -> String
describe ring =
  case ring of
    Annulus spell ->
      "ring of " ++ (Spell.describe spell)

    Enchanted n ring' ->
      "+" ++ (toString n) ++ " " ++ describe ring'

enchant : Ring -> Ring
enchant ring =
  case ring of
    Enchanted n ring' ->
      Enchanted (n+1) ring'

    _ ->
      Enchanted 1 ring

strengthBonus : Ring -> Int
strengthBonus ring =
  case ring of
    Enchanted n ring' ->
      n * (strengthBonus ring')

    Annulus spell ->
      case spell of
        Spell.Lux ->
          0

        Spell.Infuse ->
          1

visionBonus : Ring -> Int
visionBonus ring =
  case ring of
    Enchanted n ring' ->
      n * (visionBonus ring')

    Annulus spell ->
      case spell of
        Spell.Lux ->
          1

        Spell.Infuse ->
          0
