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
    Cloth -> 0.0
    Wood -> 0.2
    Leather -> 0.3
    Bronze -> 0.5
    Iron  -> 1.0
    Steel -> 2.0
    Cobalt -> 3.0
    Titanium -> 4.5
    Corundum -> 5.0
    Diamond -> 6.5
    Mandium -> 8.0

strength : Material -> Float
strength material =
  (hardness material) + 0.5

resistance : Material -> Float
resistance material =
  (hardness material)

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
