module Helm exposing (Helm, describe, absorption, enchant, cap, helmet)

type Helm = Cap
          | Helmet
          | Enchanted Int Helm

cap : Helm
cap =
  Cap

helmet : Helm
helmet =
  Helmet

describe : Helm -> String
describe helm =
  case helm of
    Cap ->
      "cap"

    Helmet ->
      "helmet"

    Enchanted n helm' ->
      "+" ++ (toString n) ++ " " ++ describe helm'

enchant : Helm -> Helm
enchant helm =
  case helm of
    Enchanted n helm' ->
      Enchanted (n+1) helm'

    _ ->
      Enchanted 1 helm

absorption : Helm -> Int
absorption helm =
  case helm of
    Cap ->
      1

    Helmet ->
      2

    Enchanted n helm' ->
      n + absorption helm'
