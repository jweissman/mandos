module Room exposing (Room, generate, overlaps, layout, filterOverlaps, network, directionBetween, distance, center)

import Point exposing (Point)
import Direction exposing (Direction(..))
import Graph exposing (Graph)
import Util

import Random

type alias Room = { origin : Point
                  , width : Int
                  , height: Int
                  }

generate : Int -> Random.Generator (List Room)
generate n =
  generate' |> Random.list n

generate' : Random.Generator Room
generate' =
  let
    width =
      Random.int 3 12 

    height =
      Random.int 4 6

    origin =
      Point.randomWithOffset {x=3,y=4} 26 14
  in
    Random.map3 create origin width height

create : Point -> Int -> Int -> Room
create point width height =
  { origin = point
  , width = width
  , height = height
  }

overlaps : Room -> Room -> Bool
overlaps a b =
  overlapsY 0 a b && overlapsX 0 a b
 
overlapsRelevantDirection : Int -> Room -> Room -> Bool
overlapsRelevantDirection n a b =
  case directionBetween a b of
    North -> overlapsX n a b
    South -> overlapsX n a b
    East  -> overlapsY n a b
    West  -> overlapsY n a b
    _ -> False

overlapsY n a b =
  not (isAbove n a b || isBelow n a b)

overlapsX n a b =
  not (isLeft n a b || isRight n a b)

isAbove : Int -> Room -> Room -> Bool
isAbove n a b =
  a.origin.y + a.height < b.origin.y - n

isBelow : Int -> Room -> Room -> Bool
isBelow n a b =
  b.origin.y + b.height < a.origin.y - n

isLeft  : Int -> Room -> Room -> Bool
isLeft n a b =
  a.origin.x + a.width < b.origin.x - n

isRight : Int -> Room -> Room -> Bool
isRight n a b =
  b.origin.x + b.width < a.origin.x - n

-- return a tuple of two lists of points -- (walls, list of points in floors)
layout : Room -> (List Point, List Point)
layout {origin,width,height} =
  layout' origin width height

layout' {x,y} width height =
  let
    walls =
      List.map (\x' -> {x=x+x',y=y}) [0..width] ++
      List.map (\x' -> {x=x+x',y=y+height}) [0..width] ++
      List.map (\y' -> {x=x,y=y+y'}) [0..height] ++
      List.map (\y' -> {x=x+width,y=y+y'}) [0..height]

    floors =
      List.concatMap (\y' ->
        List.map (\x' -> {x=x+x',y=y+y'}) [1..(width-1)]
      ) [1..(height-1)]
  in
    (walls,floors)

size : Room -> Int
size r = r.width * r.height

filterOverlaps : List Room -> List Room
filterOverlaps rooms =
  let rooms' = rooms |> List.sortBy size |> List.reverse in
  case (List.head rooms') of
    Nothing ->
      []

    Just room ->
      let
        rest =
          rooms'
          |> List.tail
          |> Maybe.withDefault []
          |> List.filter (\room' -> (not (overlaps room room')))
          |> filterOverlaps
      in
         [room] ++ rest

center : Room -> Point
center {origin,width,height} =
  { x = origin.x + width // 2
  , y = origin.y + height // 2
  }

distance : Room -> Room -> Float
distance a b =
  Point.distance (center a) (center b)

canConnect : Room -> Room -> Bool
canConnect a b =
  overlapsRelevantDirection -2 a b

network : List Room -> Maybe (Graph Room)
network rooms =
  Graph.tree distance canConnect rooms

directionBetween : Room -> Room -> Direction
directionBetween a b =
  Util.simpleDirectionBetween (center a) (center b)
