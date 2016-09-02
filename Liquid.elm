module Liquid exposing (Liquid(..), describe, water, holy)

type Liquid = Water
            | Blessed Liquid
            --| Glowing Liquid

water =
  Water

holy liquid =
  Blessed liquid

describe : Liquid -> String
describe liquid =
  case liquid of
    Water -> 
      "water"

    Blessed liquid' ->
      "holy " ++ (describe liquid')
