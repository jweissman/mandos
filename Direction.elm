module Direction exposing (Direction(..), random, describe, invert, directions, cardinalDirections)

import Random

type Direction = North 
               | South 
               | East 
               | West
               | Northeast
               | Northwest
               | Southeast
               | Southwest


cardinalDirections : List Direction
cardinalDirections =
  [ North
  , South
  , East
  , West
  ]


directions : List Direction
directions = 
  cardinalDirections ++
  [ Northeast
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
      3 -> West
      4 -> Northwest
      5 -> Northeast
      6 -> Southwest
      _ -> Southeast
    ) (Random.int 0 7)

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
