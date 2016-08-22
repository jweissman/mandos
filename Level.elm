module Level exposing (Level, init, fromRooms, turnCreature, moveCreatures, injureCreature, purge, collectCoin, isBlocked, isCoin, creatureAt, entityAt)

import Point exposing (Point)
import Direction exposing (Direction(..))
import Room exposing (Room)
import Graph exposing (Graph)

import Util
import Path exposing (Path)

import Warrior
import Creature
import Event exposing (Event)
import Entity exposing (Entity)

-- TYPE

type alias Level = { walls : List Point
                   , floors : List Point
                   , doors : List Point
                   , creatures : List Creature.Model
                   , downstairs : Point
                   , upstairs : Point
                   , coins : List Point
                   }

-- INIT

init : Level
init =
  { walls = []
  , floors = []
  , doors = []
  , creatures = []
  , coins = []
  , upstairs = origin
  , downstairs = origin
  }

origin = {x=0,y=0}

-- QUERY

isWall : Point -> Level -> Bool
isWall pt model =
  List.any (\pos -> pos == pt) model.walls

isCoin : Point -> Level -> Bool
isCoin pt model =
  List.any (\pos -> pos == pt) model.coins

isCreature : Point -> Level -> Bool
isCreature pt model =
  List.any (\pos -> pos == pt) (List.map .position model.creatures)

isDoor : Point -> Level -> Bool
isDoor pt model =
  List.any (\p -> p == pt) model.doors

isFloor : Point -> Level -> Bool
isFloor position model =
  List.any (\p -> p == position) model.floors

isStairsUp : Point -> Level -> Bool
isStairsUp position model =
  model.upstairs == position

isStairsDown : Point -> Level -> Bool
isStairsDown position model =
  model.downstairs == position

isBlocked : Point -> Level -> Bool
isBlocked position model =
  isWall position model ||
  isCreature position model -- || -- ||
  -- isStairsUp position model ||
  -- isStairsDown position model

  --isPlayer position model

isAlive livingThing =
  livingThing.hp > 0

entityAt : Point -> Warrior.Model -> Level -> Maybe Entity
entityAt point warrior model =
  if point == warrior.position then
    Just (Entity.player warrior)
  else
    if isDoor point model then
      Just (Entity.door point)
    else
      if isWall point model then
        Just (Entity.wall point)
      else
        if isCoin point model then
          Just (Entity.coin point)
        else
          if isStairsUp point model then
            Just (Entity.upstairs point)
          else
            if isStairsDown point model then
              Just (Entity.downstairs point)
            else
              let creature = model |> creatureAt point in
                case creature of
                  Just creature' ->
                    Just (Entity.monster creature')
                  Nothing ->
                    if model |> isFloor point then
                      Just (Entity.floor point)
                    else
                      Nothing

creatureAt : Point -> Level -> Maybe Creature.Model
creatureAt pt model =
  let
    creatures' =
      List.filter (\c -> c.position == pt) model.creatures
  in
    List.head creatures'

-- HELPERS (for update)

turnCreature : Creature.Model -> Direction -> Level -> Level
turnCreature creature direction model =
  let
    creatures' =
      model.creatures
      |> List.map (\c -> if c == creature then c |> Creature.turn direction else c)
  in
    { model | creatures = creatures' }

moveCreatures : Warrior.Model -> Level -> (Level, List Event, Warrior.Model)
moveCreatures player model =
  model.creatures
  |> List.foldl creatureSteps (model, [], player)

creatureSteps : Creature.Model -> (Level, List Event, Warrior.Model) -> (Level, List Event, Warrior.Model)
creatureSteps creature (model, events, player) =
  (model, events, player)
  |> creatureMoves creature
  |> creatureAttacks creature

creatureMoves creature (model, events, player) =
  let
    creatures' =
      model.creatures
      |> List.map (\c -> if c == creature && (canCreatureStep creature player model) then c |> Creature.step else c)
  in
    ({ model | creatures = creatures' }
    , events
    , player
    )

creatureAttacks creature (model, events, player) =
  let
    pos =
      creature.position
      |> Point.slide creature.direction

    dmg =
      creature.attack - player.defense
  in
    if pos == player.position then
       (model, events, player)
       |> playerTakesDamage creature dmg
       |> playerDies
    else
      (model, events, player)

playerTakesDamage creature amount (model, events, player) =
  let
    player' =
      (Warrior.takeDamage amount player)

    event =
      Event.defend creature amount
  in
    (model, event :: events, player')

playerDies (model, events, player) =
  if not (isAlive player) then
    let event = Event.death in
    (model, event :: events, player)
  else
    (model, events, player)

canCreatureStep creature player model =
  let
    next =
      creature.position
      |> Point.slide creature.direction

    isPlayer =
      player.position == next

    blocked =
      model |> (not << (isBlocked next))
  in
    not (isPlayer || blocked)

injureCreature : Creature.Model -> Int -> Level -> Level
injureCreature creature amount model =
  let
    creatures' =
      model.creatures
      |> List.map (\c ->
        if c == creature then
           c
           |> Creature.injure amount
           |> Creature.engage
        else c)
  in
    { model | creatures = creatures' }

purge : Level -> (Level, List Event)
purge model =
  let
    survivors =
      List.filter isAlive model.creatures

    killed =
      List.filter (not << isAlive) model.creatures

    deathEvents =
      List.map Event.killEnemy killed
  in
    ({ model | creatures = survivors }, deathEvents)


collectCoin : Point -> Level -> Level
collectCoin pt model =
  let
    coins' =
      model.coins
      |> List.filter (not << (\c -> c == pt))
  in
    { model | coins = coins' }

-- GENERATE

-- actually build out the rooms and corridors for a level
fromRooms : List Room -> Level
fromRooms roomCandidates =
  let
    rooms =
      roomCandidates
      |> Room.filterOverlaps
  in
    init
    |> extrudeRooms rooms
    |> connectRooms rooms
    |> extrudeStairwells
    |> dropCoins -- rooms -- drop coins along shortest path between stairwells...

extrudeRooms : List Room -> Level -> Level
extrudeRooms rooms model =
  rooms
  |> List.foldr extrudeRoom model

extrudeRoom : Room -> Level -> Level
extrudeRoom room model =
  let
      (walls,floors) =
        Room.layout room
  in
     { model | walls  = model.walls ++ walls
             , floors = model.floors ++ floors }

connectRooms : List Room -> Level -> Level
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

connectRooms' : (Room,Room) -> Level -> Level
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
        North ->
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
        model
        |> extrudeCorridor (round (Room.distance a b)) pos direction

      Nothing ->
        model

extrudeCorridor : Int -> Point -> Direction -> Level -> Level
extrudeCorridor depth pt dir model =
  extrudeCorridor' pt dir depth model

extrudeCorridor' pt dir depth model =
  let
    model' =
      { model | floors = pt :: model.floors  }
              |> addWallsAround pt
              |> removeWall pt

    --slideDir =
    --  \dir' -> Point.slide dir' pt

    --newWalls =
    --  List.map slideDir newWallDirs
    --  |> List.filter (\wall ->
    --    model.floors
    --    |> not << List.any (\floor' -> wall == floor')
    --  )

    --newWallDirs =
    --  case dir of
    --    North -> [East,West]
    --    South -> [East,West]
    --    East -> [North,South]
    --    West -> [North,South]
    --    _ -> []

    next =
      Point.slide dir pt

    foundFloor =
      (List.any (\pt' -> pt' == pt) model.floors)
  in
    if foundFloor || depth < 0 then
      model'
      |> emplaceDoor (pt |> Point.slide (Direction.invert dir))
    else
      model'
      |> extrudeCorridor' (pt |> Point.slide dir) dir (depth-1)


-- doors
emplaceDoor : Point -> Level -> Level
emplaceDoor pt model =
  { model | doors = pt :: model.doors }
          |> removeWall pt

-- stairs

extrudeStairwells : Level -> Level
extrudeStairwells model =
  let
    adjacentToFloor = (\pt ->
        (Direction.cardinalDirections
        |> List.map (\direction -> Point.slide direction pt)
        |> List.filter (\pt' -> List.member pt' (model.floors))
        |> List.length) == 1
      )

    adjacentToTwoWalls = (\pt ->
        ([[ North, South ], [ East, West ]]
        |> List.map (\ds -> 
          ds
          |> List.map (\d -> Point.slide d pt)
          |> List.filter (\pt -> (model |> isWall pt))
        )
        |> List.filter (\ls -> List.length ls > 0)
        |> List.length) == 1
      )

    candidates =
      model.walls
      |> List.filter adjacentToFloor
      |> List.filter adjacentToTwoWalls

    candidateHead =
      candidates
      |> List.head
      |> Maybe.withDefault origin

    candidateLast =
      (List.reverse candidates)
      |> List.head
      |> Maybe.withDefault origin

    upstairs =
      candidateHead

    downstairs = 
      candidateLast
      --List.head (List.tail candidates |> Maybe.withDefault origin) |> Maybe.withDefault origin
  in
    --Debug.log (toString upstairs)
    model
    |> emplaceUpstairs upstairs
    |> emplaceDownstairs downstairs

emplaceUpstairs : Point -> Level -> Level
emplaceUpstairs point model =
  { model | upstairs = point }
          |> addWallsAround point
          |> removeWall point

emplaceDownstairs : Point -> Level -> Level
emplaceDownstairs point model =
  { model | downstairs = point }
          |> addWallsAround point
          |> removeWall point

removeWall pt model =
  let
    walls' =
      model.walls |> List.filterMap (\pt' -> 
        if not (pt == pt') then Just pt' else Nothing)
  in
  { model | walls = walls' }

removeFloor pt model = 
  let
    floors' =
      model.floors 
      |> List.filterMap (\pt' -> 
        if not (pt == pt') then Just pt' else Nothing)
  in
  { model | floors = floors' }

addWallsAround pt model = 
  let
    newWalls =
      Direction.directions
      |> List.map (\d -> Point.slide d pt)
      |> List.filter (\wall ->
        not
          (model.floors
          |> List.any (\floor' -> wall == floor')) ||
          (model.walls
          |> List.any (\wall' -> wall == wall'))
      )
  in 
     { model | walls = newWalls ++ model.walls }
  
  --List.map slideDir newWallDirs
  --    |> List.filter (\wall ->
  --      model.floors
  --      |> not << List.any (\floor' -> wall == floor')
  --    )

dropCoins : Level -> Level
dropCoins model =
  let
    path' =
      model
      |> path (model.downstairs) (model.upstairs)
      |> Maybe.withDefault []
      |> List.tail |> Maybe.withDefault []
      |> List.reverse
      |> List.tail |> Maybe.withDefault []

    coins' =
      path'
      |> List.filterMap (\{x,y} -> if ((((x ^ 31) + y) % 20) < 2) then Just {x=x,y=y} else Nothing)
  in  
    { model | coins = model.coins ++ coins' }


-- pathfinding
path : Point -> Point -> Level -> Maybe Path
path dst src model =
  Path.find dst src (movesFrom model)

movesFrom : Level -> Point -> List (Point, Direction)
movesFrom model point =
  Direction.directions
  |> List.map (\direction -> (Point.slide direction point, direction))
  |> List.filter ((\p -> not (isBlocked p model)) << fst)

