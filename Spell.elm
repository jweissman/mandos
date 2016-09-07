module Spell exposing (Spell(..), describe, lux, infuse)

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
      "lux"

    Infuse ->
      "infuse"
