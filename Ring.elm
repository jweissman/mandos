module Ring exposing (Ring, describe, spell, enchant, light, power, strengthBonus, visionBonus)

import Spell exposing (Spell)
import Language exposing (Language)

type Ring = Annulus Spell
          | Enchanted Int Ring

light : Ring
light =
  Annulus Spell.lux

power : Ring
power =
  Annulus Spell.infuse

spell : Ring -> Spell
spell ring =
  case ring of
    Annulus spell' ->
      spell'

    Enchanted _ ring' ->
      spell ring

describe : Language -> Language -> Ring -> String
describe vocab language ring =
  case ring of
    Annulus spell' ->
      "ring of " ++ (Language.decode (Spell.idea spell') vocab language)

    Enchanted n ring' ->
      "+" ++ (toString n) ++ " " ++ (describe vocab language ring')

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

    Annulus spell' ->
      case spell' of
        Spell.Lux ->
          0

        Spell.Infuse ->
          1

visionBonus : Ring -> Int
visionBonus ring =
  case ring of
    Enchanted n ring' ->
      n * (visionBonus ring')

    Annulus spell' ->
      case spell' of
        Spell.Lux ->
          1

        Spell.Infuse ->
          0
