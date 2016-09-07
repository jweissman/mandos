module Liquid exposing (Liquid(..), Effect(..), describe, water, holy, lifePotion)

type Effect = GainLife

type Liquid = Water
            | Blessed Liquid
            | Potion Effect

water =
  Water

holy liquid =
  Blessed liquid

lifePotion =
  Potion GainLife

describe : Liquid -> String
describe liquid =
  case liquid of
    Water -> 
      "water"

    Blessed liquid' ->
      "holy " ++ (describe liquid')

    Potion effect ->
      case effect of
        GainLife ->
          "Life"
