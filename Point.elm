module Point exposing (Point, slide, describe, distance) --, orbit)

import Direction exposing (Direction(..), directions)

type alias Point =
  { x : Int
  , y : Int
  }

slide : Direction -> Point -> Point
slide direction point =
  case direction of
    North -> 
      { point | y = point.y - 1 }

    South -> 
      { point | y = point.y + 1 }

    West  -> 
      { point | x = point.x - 1 }

    East  -> 
      { point | x = point.x + 1 }

    Northeast ->
      point
      |> slide North
      |> slide East

    Northwest ->
      point
      |> slide North
      |> slide West

    Southeast ->
      point
      |> slide South
      |> slide East

    Southwest ->
      point
      |> slide South
      |> slide West

describe : Point -> String
describe point =
  "(" ++ (toString point.x) ++ ", " ++ (toString point.y) ++ ")"

distance : Point -> Point -> Float
distance a b =
  let
    dx = 
      toFloat (a.x - b.x)

    dy =
      toFloat (a.y - b.y)
  in
    sqrt( (dx*dx) + (dy*dy) )
