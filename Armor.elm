module Armor exposing (Armor, absorption, describe, tunic, suit)

--import Material exposing (Material)

import String

type Armor = Suit
           | Tunic
           | Plate
           | Enchanted Int Armor

tunic : Armor
tunic = 
  Tunic

suit : Armor
suit =
  Suit

plate : Armor
plate =
  Plate

enchant : Armor -> Armor
enchant armor =
  case armor of
    Enchanted n armor' ->
      Enchanted (n+1) armor'
    
    _ ->
      Enchanted 1 armor

absorption : Armor -> Int
absorption armor =
  case armor of
    Tunic -> 
      2

    Suit -> 
      5

    Plate -> 
      7

    Enchanted n armor' ->
      n + absorption armor'

describe : Armor -> String
describe armor =
  case armor of
    Suit ->
      "suit"

    Tunic ->
      "tunic"

    Plate ->
      "plate"

    Enchanted n armor' ->
      "+" ++ (toString n) ++ " " ++ describe armor'
