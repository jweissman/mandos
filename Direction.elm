module Direction exposing (Direction(..), random, describe, invert, directions)

import Random

type Direction = North | South | East | West

directions : List Direction
directions = 
  [ North, South, East, West ]

random : Random.Generator Direction
random =
  Random.map (\i ->
    case i of
      0 -> South
      1 -> North
      2 -> East
      _ -> West
    ) (Random.int 0 3)

describe : Direction -> String
describe direction =
  case direction of
    North -> "north"
    South -> "south"
    East  -> "east"
    West  -> "west"

invert : Direction -> Direction
invert direction =
  case direction of
    North -> South
    South -> North
    East -> West
    West -> East
