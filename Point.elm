module Point exposing (Point, slide, describe, orbit)

import Direction exposing (Direction(..), directions)

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

describe : Point -> String
describe point =
  "(" ++ (toString point.x) ++ ", " ++ (toString point.y) ++ ")"

orbit : Point -> List Point
orbit point =
  List.map (slide point) directions
