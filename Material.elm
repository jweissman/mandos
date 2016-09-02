module Material exposing (Material, describe, strength, resistance, iron, wood, steel, leather, bronze, mandium, cloth)

type Material = Cloth
              | Leather
              | Wood
              | Bronze
              | Iron
              | Steel
              | Mandium

cloth =
  Cloth

bronze =
  Bronze

iron =
  Iron

wood =
  Wood

steel =
  Steel

leather =
  Leather

mandium =
  Mandium

describe : Material -> String
describe material =
  case material of
    Bronze -> "bronze"
    Cloth -> "cloth"
    Iron -> "iron"
    Leather -> "leather"
    Mandium -> "mandium"
    Steel -> "steel"
    Wood -> "wooden"

strength : Material -> Float
strength material =
  case material of
    Cloth -> 0.2
    Leather -> 0.4
    Wood  -> 1.0
    Bronze -> 1.6
    Iron  -> 2.5
    Steel -> 4.7
    Mandium -> 6.4

resistance : Material -> Float
resistance material =
  case material of
    Cloth -> 1.0
    Wood  -> 1.5
    Leather -> 2.0
    Bronze -> 2.5
    Iron  -> 3.5
    Steel -> 4.8
    Mandium -> 7.2
