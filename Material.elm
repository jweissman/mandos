module Material exposing (Material, describe, strength, resistance, forWeaponry, forArmor)

import ChallengeRating exposing (ChallengeRating(..))

type Material = Cloth
              | Leather
              | Wood
              | Bronze
              | Iron
              | Steel
              | Cobalt
              | Titanium
              | Corundum
              | Diamond
              | Mandium

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
    Cobalt -> "cobalt"
    Corundum -> "corundum"
    Titanium -> "titanium"
    Diamond -> "diamond"

hardness : Material -> Float 
hardness material =
  case material of
    Cloth -> 1.0
    Leather -> 1.2
    Wood -> 1.3
    Bronze -> 1.4
    Iron  -> 1.5
    Steel -> 2.7
    Cobalt -> 4.0
    Titanium -> 6.7
    Corundum -> 7.4
    Diamond -> 8.5
    Mandium -> 10.0

strength : Material -> Float
strength material =
  ((hardness material)^2) + 0.5

resistance : Material -> Float
resistance material =
  ((1+(hardness material)) * 2)

forArmor : ChallengeRating -> Material
forArmor rating =
  case rating of
    Beginner ->
      Leather

    Easy ->
      Iron

    Moderate ->
      Steel

    Hard ->
      Titanium

    Impossible ->
      Mandium

forWeaponry : ChallengeRating -> Material
forWeaponry rating =
  case rating of
    Beginner ->
      Bronze

    Easy ->
      Iron

    Moderate ->
      Steel

    Hard ->
      Corundum

    Impossible ->
      Diamond
