module Spell exposing (Spell(..), describe, lux)

type Spell = Lux
           | Infuse

lux =
  Lux

infuse =
  Infuse

describe : Spell -> String
describe spell =
  case spell of
    Lux ->
      "Illumination"

    Infuse ->
      "Enchantment"
