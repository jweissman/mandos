module Geometry exposing (Direction(..), Point, slide, aDirection, randomDirection, describeDirection)

import Random

type Direction = North | South | East | West

type alias Point =
  { x : Int
  , y : Int
  }

slide : Point -> Direction -> Point
slide point direction =
  case direction of
    North -> { point | y = point.y - 1 }
    South -> { point | y = point.y + 1 }
    West  -> { point | x = point.x - 1 }
    East  -> { point | x = point.x + 1 }

aDirection : Direction
aDirection = North

randomDirection : Random.Generator Direction
randomDirection =
  Random.map (\i ->
    case i of
      0 -> South
      1 -> North
      2 -> East
      _ -> West
    ) (Random.int 0 3)

describeDirection : Direction -> String
describeDirection direction =
  case direction of
    North -> "north"
    South -> "south"
    East  -> "east"
    West  -> "west"
