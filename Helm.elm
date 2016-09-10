module Helm exposing (Helm, describe, enchant, cap)

type Helm = Cap
          | Enchanted Int Helm

cap : Helm
cap =
  Cap

describe : Helm -> String
describe helm =
  case helm of
    Cap ->
      "cap"

    Enchanted n helm' ->
      "+" ++ (toString n) ++ " " ++ describe helm'

enchant : Helm -> Helm
enchant helm =
  case helm of
    Enchanted n helm' ->
      Enchanted (n+1) helm'

    _ ->
      Enchanted 1 helm
