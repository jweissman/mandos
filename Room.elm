module Room exposing (Room, generate, overlaps, layout, filterOverlaps)

import Point exposing (Point)
import Random

type Id = Id Int

type Room = Room { origin : Point
                  , width : Int
                  , height: Int
                  , connections : List Room
                  --, id : Id
                  --, connections : List Id
                  }

generate : Int -> Random.Generator (List Room)
generate n =
  generate' |> Random.list n

generate' : Random.Generator Room
generate' =
  Random.map3 (create) (Point.randomWithOffset {x=3,y=4} 26 12) (Random.int 4 8) (Random.int 4 5)

create : Point -> Int -> Int -> Room
create point width height =
  Room { origin = point
  , width = width
  , height = height
  , connections = []
  --, id = (Id id)
  }

overlaps : Room -> Room -> Bool
overlaps room_a (Room {origin,width,height}) =
  overlaps' room_a origin width height

overlaps' : Room -> Point -> Int -> Int -> Bool
overlaps' (Room {origin,width,height}) b b_width b_height =
  let
    a =
      origin

    --b =
    --  room_b.origin

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
layout (Room {origin,width,height}) =
  layout' origin width height

layout' {x,y} width height =
  let
    walls =
      List.map (\x' -> {x=x+x',y=y}) [0..width] ++
      List.map (\x' -> {x=x+x',y=y+height}) [0..width] ++
      List.map (\y' -> {x=x,y=y+y'}) [0..height] ++
      List.map (\y' -> {x=x+width,y=y+y'}) [0..height]

    floors =
      List.concatMap (\y' -> (List.map (\x' -> {x=x+x',y=y+y'}) [1..(width-1)])) [1..(height-1)]
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
