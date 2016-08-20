module Dungeon exposing (Dungeon, generate) --, createLevel)

import World
import Point exposing (Point)

import Room exposing (Room) --(..))

import Random

type alias Dungeon = List DungeonLevel

type alias DungeonLevel = { walls : List Point
                          , floors : List Point
                          }

generate : Random.Generator Dungeon
generate =
  Random.list 1 (Random.map createLevel (Room.generate 100))

-- create the walls and floors for a level
createLevel : List Room -> DungeonLevel
createLevel roomCandidates =
  let
    rooms =
      roomCandidates
      |> Room.filterOverlaps

    emptyLevel =
      { walls = []
      , floors = []
      }
  in
    emptyLevel
    |> extrudeRooms rooms
    --|> connectRooms rooms

extrudeRooms : List Room -> DungeonLevel -> DungeonLevel
extrudeRooms rooms model =
  rooms
  |> List.foldr extrudeRoom model

extrudeRoom : Room -> DungeonLevel -> DungeonLevel
extrudeRoom room model =
  let
      (walls,floors) =
        Room.layout room
  in
     { model | walls  = model.walls ++ walls
             , floors = model.floors ++ floors }

-- okay so algo idea:
-- for each unconnected room, connect it to the closest connected room?
--  until we're entirely connected...?
connectRooms : List Room -> DungeonLevel -> DungeonLevel
connectRooms rooms model =
  let
    graph =
      Room.network rooms

    model' =
      model
      |> extrudeCorridors graph

  in
    Debug.log (toString graph)
    connectRooms' [] rooms model

extrudeCorridors graph model =
  model

--connectRooms' : List Room -> List Room -> DungeonLevel -> DungeonLevel
--connectRooms' connected disconnected model =
--  let
--    room =
--      disconnected
--      |> List.head
--  in
--    case room of
--      Nothing -> 
--        model -- we're done!
--
--      Just room ->
--        if List.length connected == 0 then
--          -- find closest room
--        else
--          -- find the closest *connected* room
--
--        -- either way recurse


--closestRoom : Room -> List Rooms -> Room

