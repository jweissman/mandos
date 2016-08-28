module Room exposing (Room, generate, overlaps, layout, filterOverlaps, network, directionBetween, distance, center)

import Point exposing (Point)
import Direction exposing (Direction(..))
import Graph exposing (Graph)
import Util
import Configuration

import Random

type RoomType = EmptyRoom
              | TreasureHoard
              --| MonsterCamp
              --| DragonLair

type alias Room = { origin : Point
                  , width : Int
                  , height: Int
                  --, kind : RoomType
                  }

generate : Int -> Random.Generator (List Room)
generate n =
  generate' |> Random.list n

generate' : Random.Generator Room
generate' =
  let
    maxRoomSize = 
      16

    vWidth =
      Configuration.viewWidth // 2

    vHeight =
      Configuration.viewHeight // 2

    width =
      Random.int 3 maxRoomSize

    height =
      Random.int 3 maxRoomSize

    origin =
      Point.randomWithOffset (3,4) (vWidth-maxRoomSize) (vHeight-maxRoomSize)

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
  overlapsY -1 a b && overlapsX -1 a b
 
overlapsRelevantDirection : Int -> Room -> Room -> Bool
overlapsRelevantDirection n a b =
  case directionBetween a b of
    North -> overlapsX n a b --&& not (overlapsY n a b)
    South -> overlapsX n a b --&& not (overlapsY n a b)
    East  -> overlapsY n a b --&& not (overlapsX n a b)
    West  -> overlapsY n a b --&& not (overlapsX n a b)
    _ -> False

overlapsY n a b =
  not (isAbove n a b || isBelow n a b)

overlapsX n a b =
  not (isLeft n a b || isRight n a b)

isAbove : Int -> Room -> Room -> Bool
isAbove n a b =
  let 
    (_,ay) = a.origin 
    (_,by) = b.origin
  in
  ay + a.height < by - n

isBelow : Int -> Room -> Room -> Bool
isBelow n a b =
  let 
    (_,ay) = a.origin 
    (_,by) = b.origin
  in
  by + b.height < ay - n

isLeft  : Int -> Room -> Room -> Bool
isLeft n a b =
  let 
    (ax,_) = a.origin 
    (bx,_) = b.origin
  in
  ax + a.width < bx - n

isRight : Int -> Room -> Room -> Bool
isRight n a b =
  let 
    (ax,_) = a.origin 
    (bx,_) = b.origin
  in
  bx + b.width < ax - n

-- return a tuple of two lists of points -- (walls, list of points in floors)
layout : Room -> (List Point, List Point)
layout {origin,width,height} =
  layout' origin width height

layout' (x,y) width height =
  let
    walls =
      Point.perimeter (x,y) width height

    floors =
      Point.grid (x+1,y+1) (width-2) (height-2)
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
  let (x,y) = origin in
  ( x + width // 2 , y + height // 2 )

distance : Room -> Room -> Float
distance a b =
  Point.distance (center a) (center b)

canConnect : Room -> Room -> Bool
canConnect a b =
  overlapsRelevantDirection -2 a b

network : List Room -> Maybe (Graph Room)
network rooms =
  -- would think this shoudl filter unconnectable rooms!!
  Graph.tree distance canConnect rooms

directionBetween : Room -> Room -> Direction
directionBetween a b =
  Point.towards' (center a) (center b)
