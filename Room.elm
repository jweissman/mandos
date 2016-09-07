module Room exposing (Room, Purpose(..), generate, overlaps, layout, filterOverlaps, network, directionBetween, distance, center, corridor, assign, armory, barracks, library)

import Point exposing (Point)
import Direction exposing (Direction(..))
import Graph exposing (Graph)
import Util
import Configuration
import Bresenham

import Random
import Set exposing (Set)

type Purpose = Armory
             | Barracks
             | Library

armory =
  Armory

barracks =
  Barracks

library =
  Library

type alias Room = { origin : Point
                  , width : Int
                  , height : Int
                  , purpose : Maybe Purpose
                  , id : Int
                  }

generate : Int -> Random.Generator (List Room)
generate n =
  generate' |> Random.list n

generate' : Random.Generator Room
generate' =
  let
    width' =
      Configuration.maxRoomWidth

    height' =
      Configuration.maxRoomHeight

    vWidth =
      Configuration.viewWidth // 2

    vHeight =
      Configuration.viewHeight // 2

    width =
      Random.int 4 width'

    height =
      Random.int 4 height'

    origin =
      Point.randomWithOffset (3,4) (vWidth-width') (vHeight-height')

  in
    Random.map3 create origin width height

create : Point -> Int -> Int -> Room
create point width height =
  { origin = point
  , width = width
  , height = height
  , purpose = Nothing
  , id = -1
  }

assign purpose room =
  { room | purpose = Just purpose }

overlaps : Room -> Room -> Bool
overlaps a b =
  overlapsY -1 a b && overlapsX -1 a b

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

layout : Room -> (Set Point, Set Point)
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

corridor : Room -> Room -> List Point
corridor a b =
  case corridorEndpoints a b of
    Just (pos,pos') ->
      Bresenham.line pos pos'

    Nothing ->
      []

corridorEndpoints : Room -> Room -> Maybe (Point,Point)
corridorEndpoints a b =
  let
    direction =
      directionBetween a b
      |> Direction.invert

    (ax,ay) =
      a.origin

    (bx,by) =
      b.origin

    xOverlapStart =
      (max ax bx) + 1

    xOverlapEnd =
      (min (ax+a.width) (bx+b.width)) - 1

    xOverlapRange =
      [(xOverlapStart)..(xOverlapEnd)]

    sampleOverlap = \overlap ->
       Util.sample a.height bx -1 overlap

    yOverlapStart =
      (max ay by) + 1

    yOverlapEnd =
      (min (ay+a.height) (by+b.height)) - 1

    yOverlapRange =
      [(yOverlapStart)..(yOverlapEnd)]
  in
    case direction of
      North ->
        Just ((sampleOverlap xOverlapRange, ay),
             (sampleOverlap xOverlapRange, by+b.height))

      South ->
        Just ((sampleOverlap xOverlapRange, ay+a.height),
              (sampleOverlap xOverlapRange, by))

      East ->
        Just ((ax+a.width, sampleOverlap yOverlapRange),
              (bx, sampleOverlap yOverlapRange))

      West ->
        Just ((ax, sampleOverlap yOverlapRange),
              (bx+b.width, sampleOverlap yOverlapRange))

      _ ->
        Nothing

canConnect : Room -> Room -> Bool
canConnect a b =
  overlapsRelevantDirection -2 a b

network : List Room -> Maybe (Graph Room)
network rooms =
  Graph.tree distance canConnect rooms

directionBetween : Room -> Room -> Direction
directionBetween a b =
  Point.towards' (center a) (center b)
