module Weapon exposing (Weapon, damage, describe, averageDamage, dagger, sword, axe, enchant, threatRange)

import Util
import Point exposing (Point)
import Direction exposing (Direction)

import String

type Weapon = Sword
            | Axe
            | Dagger
            | Enchanted Int Weapon

axe : Weapon
axe =
  Axe

dagger : Weapon
dagger =
  Dagger

sword : Weapon
sword =
  Sword

enchant : Weapon -> Weapon
enchant weapon =
  case weapon of
    Enchanted n weapon' ->
      Enchanted (n+1) weapon'

    _ ->
      Enchanted 1 weapon

threatRange : Point -> Direction -> Weapon -> List Point
threatRange pt dir weapon =
  case weapon of
    Axe ->
      Direction.directions
      |> List.map (\dir -> pt |> Point.slide dir)

    Enchanted n weapon' ->
      threatRange pt dir weapon'

    _ ->
      [ pt |> Point.slide dir ]

averageDamage : Weapon -> Int
averageDamage weapon =
  let
    dmgRange =
      (damageRange weapon)

    midpoint =
      (List.length dmgRange) // 2

    avg =
      Util.getAt dmgRange midpoint
      |> Maybe.withDefault 1
  in
    avg

describe : Weapon -> String
describe weapon =
  case weapon of
    Sword ->
      "sword"

    Axe ->
      "axe"

    Dagger ->
      "dagger"

    Enchanted n weapon' ->
      "+" ++ (toString n) ++ " " ++ (describe weapon')

damage : Int -> Int -> Weapon -> Int
damage m n weapon =
  Util.sample m n 1 (damageRange weapon)

damageRange weapon =
  case weapon of
    Sword ->
      [2..8]

    Dagger ->
      [1..4]

    Axe ->
      [3..5]

    Enchanted n weapon' ->
      (damageRange weapon')
      |> List.map (\x -> x + n)
