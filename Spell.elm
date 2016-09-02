module Spell exposing (Spell(..), describe, lux)

type Spell = Lux

lux =
  Lux

describe : Spell -> String
describe spell =
  case spell of
    Lux ->
      "Illumination"
