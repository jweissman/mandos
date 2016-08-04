module Point exposing (Point, slide)

import Direction exposing (Direction(..))

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

