module Mapmaking exposing (generateDungeon)

import World
import Point exposing (Point)

import Random

type alias Room = { origin : Point
                  , width : Int
                  , height: Int
                  }

generateDungeon : Random.Generator World.Model
generateDungeon =
  Random.map createDungeon generateRooms

createDungeon : List Room -> World.Model
createDungeon roomCandidates =
  let
    rooms =
      roomCandidates
      |> removeOverlappingRooms
  in
    World.init
    |> extrudeRooms rooms
    |> connectRooms rooms
    
doesOverlap : Room -> Room -> Bool
doesOverlap room_a room_b =
  let
    a = 
      room_a.origin

    b =
      room_b.origin

    a' =
      { a | x = a.x + room_a.width
          , y = a.y + room_a.height
      }

    b' =
      { b | x = b.x + room_b.width
          , y = b.y + room_b.height
      }

    disjointY =
      a.x < b'.x && a'.x > b.x

    disjointX =
      a.y < b'.y && a'.y > b.y
  in
    not (disjointX && disjointY)

--doesNotOverlap room_a room_b = 
--  not (doesOverlap room_a room_b)

--if (RectA.X1 < RectB.X2 && RectA.X2 > RectB.X1 &&
--    RectA.Y1 < RectB.Y2 && RectA.Y2 > RectB.Y1) 
removeOverlappingRooms : List Room -> List Room
removeOverlappingRooms rooms =
  --if List.length == 0 then [] else
  case (List.head rooms) of
    Nothing -> 
      []

    Just room ->
      let 
        rest = 
          rooms
          |> List.tail
          |> Maybe.withDefault []
          |> List.filter (doesOverlap room) --(\room' -> not (jdoesOverlap room)
          |> removeOverlappingRooms
      in
         Debug.log (toString (List.length rooms))
         [room] ++ rest

extrudeRooms : List Room -> World.Model -> World.Model
extrudeRooms rooms model =
  List.foldr (extrudeRoom) model rooms

extrudeRoom : Room -> World.Model -> World.Model
extrudeRoom room model =
  let
      (walls,floors) =
        layoutRoom room.origin room.width room.height
  in
     { model | walls  = model.walls ++ walls
             , floors = model.floors ++ floors } 

connectRooms : List Room -> World.Model -> World.Model
connectRooms rooms model = 
  model

-- just a little four walled room for now...
layoutRoom {x,y} width height =
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

generateRooms : Random.Generator (List Room)
generateRooms =
  Random.list 100 generateRoom

generateRoom : Random.Generator Room
generateRoom =
  Random.map3 createRoom (randomPoint 35 15) (Random.int 4 8) (Random.int 4 5)

createRoom : Point -> Int -> Int -> Room
createRoom point width height =
  { origin = point
  , width = width
  , height = height
  }

randomPoint : Int -> Int -> Random.Generator Point
randomPoint width height =
  Random.map2 (\x y -> {x=x,y=y}) (Random.int 2 width) (Random.int 3 height)
