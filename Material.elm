module Material exposing (Material, describe, strength, resistance, iron, wood, steel, leather)

type Material = Leather
              | Iron
              | Wood
              | Steel

iron =
  Iron

wood =
  Wood

steel =
  Steel

leather =
  Leather

describe : Material -> String
describe material =
  case material of
    Leather -> "leather"
    Wood -> "wooden"
    Iron -> "iron"
    Steel -> "steel"

strength : Material -> Float
strength material =
  case material of
    Leather -> 0.1
    Wood  -> 0.8
    Iron  -> 1.0
    Steel -> 1.25

resistance : Material -> Float
resistance material =
  case material of
    Wood  -> 0.2
    Leather -> 1.0
    Iron  -> 1.4
    Steel -> 2.8
