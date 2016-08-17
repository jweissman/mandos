module Direction exposing (Direction(..), random, describe, invert, directions) --, between)

import Random

type Direction = North 
               | South 
               | East 
               | West
               | Northeast
               | Northwest
               | Southeast
               | Southwest

directions : List Direction
directions = 
  [ North
  , South
  , East
  , West
  , Northeast
  , Northwest
  , Southeast
  , Southwest 
  ]

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
    Northeast -> "northeast"
    Northwest -> "northwest"
    Southeast -> "southeast"
    Southwest -> "southwest"

invert : Direction -> Direction
invert direction =
  case direction of
    North -> South
    South -> North
    East -> West
    West -> East
    Northeast -> Southwest
    Northwest -> Southeast
    Southwest -> Northeast
    Southeast -> Northwest
