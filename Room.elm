module Room exposing (Room, generate, overlaps, layout, filterOverlaps)

import Point exposing (Point)
import Random

import Graph

type alias Room = { origin : Point
                  , width : Int
                  , height: Int
                  }

generate : Int -> Random.Generator (List Room)
generate n =
  generate' |> Random.list n

generate' : Random.Generator Room
generate' =
  Random.map3 create (Point.randomWithOffset {x=3,y=4} 20 12) (Random.int 4 8) (Random.int 4 5)

create : Point -> Int -> Int -> Room
create point width height =
  Room { origin = point
  , width = width
  , height = height
  , connections = []
  }

overlaps : Room -> Room -> Bool
overlaps room_a ({origin,width,height}) =
  overlaps' origin width height room_a

overlaps' : Point -> Int -> Int -> Room -> Bool
overlaps' b b_width b_height ({origin,width,height}) =
  let
    a =
      origin

    a' =
      { a | x = a.x + width
          , y = a.y + height
      }

    b' =
      { b | x = b.x + b_width
          , y = b.y + b_height
          }

    disjointX =
      a.x < b'.x && a'.x > b.x

    disjointY =
      a.y < b'.y && a'.y > b.y
  in
    not (disjointX && disjointY)

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

filterOverlaps : List Room -> List Room
filterOverlaps rooms =
  case (List.head rooms) of
    Nothing ->
      []

    Just room ->
      let
        rest =
          rooms
          |> List.tail
          |> Maybe.withDefault []
          |> List.filter (overlaps room)
          |> filterOverlaps
      in
         Debug.log (toString (List.length rooms))
         [room] ++ rest

distance : Room -> Room -> Float
distance {origin,width,height} room =
  let
    cx = 
      origin.x + width

    cx' =
      room.origin.x + room.width

    cy =
      origin.y + height

    cy' =
      room.origin.y + room.height

  in
    Point.distance {x=cx,y=cy} {x=cx',y=cy'}

network : List Room -> Graph Room
network rooms =
  Graph.network Room.distance rooms
