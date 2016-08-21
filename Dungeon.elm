module Dungeon exposing (Dungeon, generate) --, createLevel)

import World
import Point exposing (Point)

import Room exposing (Room) --(..))
import Graph

import Direction exposing (Direction(..))

import Random
import Util

type alias Dungeon = List DungeonLevel

type alias DungeonLevel = { walls : List Point
                          , floors : List Point
                          --, doors : List Point,
                          --, upstairs : Point
                          --, downstars : Point
                          --, coins : List Point
                          }

generate : Random.Generator Dungeon
generate =
  Random.list 50 (Random.map createLevel (Room.generate 50))

-- create the walls and floors for a level
createLevel : List Room -> DungeonLevel
createLevel roomCandidates =
  let
    rooms =
      roomCandidates
      |> Room.filterOverlaps
      --|> Room.filterConnectable

    emptyLevel =
      { walls = []
      , floors = []
      }
  in
    emptyLevel
    |> extrudeRooms rooms
    |> connectRooms rooms
    --|> dropCoins rooms

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

connectRooms : List Room -> DungeonLevel -> DungeonLevel
connectRooms rooms model =
  let
    maybeNetwork =
      Room.network rooms

    model' =
      model

  in
    case maybeNetwork of
      Just graph ->
        graph
        |> Graph.fold connectRooms' model

      Nothing ->
        model

connectRooms' : (Room,Room) -> DungeonLevel -> DungeonLevel
connectRooms' (a, b) model =
  let
    direction =
      Direction.invert (Room.directionBetween a b)

    xOverlapStart =
      (max a.origin.x b.origin.x) + 1

    xOverlapEnd =
      (min (a.origin.x+a.width) (b.origin.x+b.width)) - 1

    xOverlapRange =
      [(xOverlapStart)..(xOverlapEnd)]

    -- deterministic but pseudorand sample of the range
    sampleOverlap = \overlap ->
       Util.getAt overlap ((a.height ^ 31 + b.origin.x) % (List.length overlap))
       |> Maybe.withDefault -1

    yOverlapStart =
      (max a.origin.y b.origin.y) + 1

    yOverlapEnd =
      (min (a.origin.y+a.height) (b.origin.y+b.height)) - 1

    yOverlapRange =
      [(yOverlapStart)..(yOverlapEnd)]
    
    startPosition =
      case direction of
        North -> -- a's north wall where it intersects with b's width...
          Just {x=(sampleOverlap xOverlapRange), y=a.origin.y}

        South ->
          Just {x=(sampleOverlap xOverlapRange), y=a.origin.y+a.height}

        East -> 
          Just {x=a.origin.x+a.width, y=(sampleOverlap yOverlapRange)}

        West ->
          Just {x=a.origin.x, y=(sampleOverlap yOverlapRange)}

        _ -> 
          Nothing
  in
    case startPosition of
      Just pos -> 
        --Debug.log ((toString xOverlapRange) ++ ", y overlap: " ++ (toString yOverlapRange))
        --Debug.log ("Connecting rooms " ++ (toString a) ++ " and " ++ (toString b) ++ " -> " ++ (toString pos)  ++ (toString direction))
        model 
        |> extrudeCorridor (round (Room.distance a b)) pos direction

      Nothing ->
        model

extrudeCorridor : Int -> Point -> Direction -> DungeonLevel -> DungeonLevel
extrudeCorridor depth pt dir model =
  extrudeCorridor' pt dir depth model

extrudeCorridor' pt dir depth model =
  let 
    model' =
      { model | 
        floors = pt :: model.floors
      , walls  = newWalls ++ wallsWithoutPoint 
      }

    wallsWithoutPoint =
      model.walls
      |> List.filterMap (\pt' -> if not (pt == pt') then Just pt' else Nothing)

    wallInDir = 
      \dir' -> Point.slide dir' pt 

    newWalls = 
      List.map wallInDir newWallDirs
      |> List.filter (\wall -> not (List.any (\pt' -> wall == pt') model.floors))

    newWallDirs =
      case dir of
        North -> [East,West] 
        South -> [East,West]
        East -> [North,South]
        West -> [North,South]
        _ -> []

    next =
      Point.slide dir pt

    foundFloor =
      (List.any (\pt' -> pt' == pt) model.floors)
  in
    if foundFloor || depth < 0 then
      model
    else -- keep going
      --Debug.log ("extrude again! " ++ (toString pt))
      model'
      |> extrudeCorridor' (pt |> Point.slide dir) dir (depth-1)

