module Point exposing (Point, x, y, slide, describe, distance, random, randomWithOffset, code, perimeter, grid, isAdjacent, towards, towards', adjacent, fromMouse)

import Direction exposing (Direction(..), directions)

import Random
import Set exposing (Set)
import Mouse
import Configuration

type alias Point = ( Int, Int )

x (x',_) = x'
y (_,y') = y'

adjacent : Point -> List Point
adjacent pt =
  Direction.directions
  |> List.map (\dir -> pt |> slide dir)

isAdjacent a b =
  adjacent a
  |> List.member b

slide : Direction -> Point -> Point
slide direction (x,y) =
  case direction of
    North ->
      (x, y - 1)

    South ->
      (x, y + 1)

    West  ->
      (x - 1, y)

    East  ->
      (x + 1, y)

    Northeast ->
      (x,y)
      |> slide North
      |> slide East

    Northwest ->
      (x,y)
      |> slide North
      |> slide West

    Southeast ->
      (x,y)
      |> slide South
      |> slide East

    Southwest ->
      (x,y)
      |> slide South
      |> slide West

describe : Point -> String
describe (x,y) =
  "(" ++ (toString x) ++ ", " ++ (toString y) ++ ")"

distance : Point -> Point -> Float
distance (ax,ay) (bx,by) =
  let
    dx =
      toFloat (ax - bx)

    dy =
      toFloat (ay - by)
  in
    sqrt( (dx*dx) + (dy*dy) )

random : Int -> Int -> Random.Generator Point
random width height =
  Random.map2 (\x y -> (x,y)) (Random.int 0 width) (Random.int 0 height)


randomWithOffset : Point -> Int -> Int -> Random.Generator Point
randomWithOffset (x,y) width height =
  Random.map2 (\x' y' -> (x+x',y+y')) (Random.int 0 width) (Random.int 0 height)

code : Point -> Int
code (x,y) =
  (x * 10000) + y

perimeter : Point -> Int -> Int -> Set Point
perimeter (x,y) width height =
  let ls =
  List.map (\x' -> (x+x',y)) [0..width] ++
  List.map (\x' -> (x+x',y+height)) [0..width] ++
  List.map (\y' -> (x,y+y')) [0..height] ++
  List.map (\y' -> (x+width,y+y')) [0..height]
  in
    Set.fromList ls

grid : Point -> Int -> Int -> Set Point
grid (x,y) width height =
  List.concatMap (\y' ->
    List.map (\x' -> (x+x',y+y')) [0..(width)]
  ) [0..(height)]
  |> Set.fromList

towards : Point -> Point -> Direction
towards (ax,ay) (bx,by) =
  if (ax > bx) && (ay > by) then
     Southeast
  else
    if (ax < bx) && (ay > by) then
      Southwest
    else
      if (ax > bx) && (ay < by) then
        Northeast
      else
        if (ax < bx) && (ay < by) then
          Northwest
        else
          towards' (ax,ay) (bx,by)

towards' : Point -> Point -> Direction
towards' (ax,ay) (bx,by) =
  let
    dx =
      abs (ax - bx)
    dy =
      abs (ay - by)

  in
    if dx > dy then
      if (ax > bx) then
        East
      else
        West
    else
      if (ay > by) then
        South
      else
        North


fromMouse : Mouse.Position -> Point
fromMouse {x,y} =
  let scale = Configuration.viewScale in
  ( x//scale
  , (y//scale)+1 -- ignore top bar ...
  )


