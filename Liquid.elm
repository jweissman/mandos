module Liquid exposing (Liquid, describe, water)

type Liquid = Water
            --| Acid
            --| Blood
            --| Oil

water = 
  Water

describe : Liquid -> String
describe liquid =
  case liquid of
    Water -> "water"
