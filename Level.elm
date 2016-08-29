module Level exposing (Level, init, fromRooms, finalize, moveCreatures, injureCreature, purge, collectCoin, isCoin, isCrystal, isEntrance, isCreature, creatureAt, entitiesAt, playerSees, liberateCrystal)


import Point exposing (Point)
import Direction exposing (Direction(..))
import Room exposing (Room)
import Graph exposing (Graph)

import Util
import Path
import Configuration

import Warrior
import Creature
import Event exposing (Event)
import Entity exposing (Entity)

import Set exposing (Set)


-- TYPE

type alias Level = { walls : Set Point
                   , floors : Set Point
                   , doors : Set Point
                   , coins : Set Point
                   , creatures : List Creature.Model
                   , downstairs : Maybe Point
                   , upstairs : Maybe Point
                   , crystal : Maybe (Point, Bool)
                   , entrance : Maybe (Point, Bool)

                   , rooms : List Room
                   , viewed : List Point
                   }

-- INIT

init : Level
init =
  { walls = Set.empty
  , floors = Set.empty
  , doors = Set.empty
  , coins = Set.empty
  , creatures = []
  , upstairs = Nothing --origin
  , downstairs = Nothing --origin
  , crystal = Nothing
  , entrance = Nothing
  , rooms = []
  , viewed = []
  }

-- should probably spawn creatures here too
-- since we finally know what level we are!
finalize : Int -> Level -> Level
finalize depth model =
  model
  |> finalizeEntrance depth
  |> finalizeCrystal depth
  |> spawnCreatures depth -- rooms

finalizeEntrance depth model =
  if depth == 0 then
    case model.upstairs of
      Nothing -> model
      Just pt ->
        model
        |> emplaceEntrance pt
  else
    model

finalizeCrystal depth model =
  if depth == (Configuration.levelCount - 1) then
    case model.downstairs of
      Nothing -> model
      Just pt ->
        model
        |> emplaceCrystal pt
  else
    model

origin = (0,0)

-- QUERY

isWall : Point -> Level -> Bool
isWall pt model =
  Set.member pt model.walls

isCoin : Point -> Level -> Bool
isCoin pt model =
  Set.member pt model.coins

isCreature : Point -> Level -> Bool
isCreature pt model =
  List.member pt (List.map .position model.creatures)

isDoor : Point -> Level -> Bool
isDoor pt model =
  Set.member pt model.doors

isFloor : Point -> Level -> Bool
isFloor pt model =
  Set.member pt model.floors

isStairsUp : Point -> Level -> Bool
isStairsUp position model =
  case model.upstairs of
    Just pt ->
      position == pt
    Nothing ->
      False

isStairsDown : Point -> Level -> Bool
isStairsDown position model =
  case model.downstairs of
    Just pt ->
      position == pt
    Nothing ->
      False
  --model.downstairs == position

isEntrance : Point -> Level -> Bool
isEntrance position model =
  case model.entrance of
    Just (pt,_) ->
      position == pt
    Nothing ->
      False

isCrystal : Point -> Level -> Bool
isCrystal position model =
  case model.crystal of
    Just (pt,_) ->
      position == pt
    Nothing ->
      False

hasBeenViewed : Point -> Level -> Bool
hasBeenViewed point model =
  List.member point model.viewed

isAlive livingThing =
  livingThing.hp > 0

entitiesAt : Point -> Level -> List Entity
entitiesAt point model =
  let
    monster =
      let creature = model |> creatureAt point in
      case creature of
        Just creature' ->
          Just (Entity.monster creature')
        Nothing ->
          Nothing

    door =
      if isDoor point model then
        Just (Entity.door point)
      else
        Nothing

    wall =
      if isWall point model then
        Just (Entity.wall point)
      else
        Nothing

    floor =
      if isFloor point model then
        Just (Entity.floor point)
      else
        Nothing

    coin =
      if isCoin point model then
        Just (Entity.coin point)
      else
        Nothing

    downstairs =
      if isStairsDown point model then
        Just (Entity.downstairs point)
      else
        Nothing

    upstairs =
      if isStairsUp point model then
        Just (Entity.upstairs point)
      else
        Nothing

    entrance =
      if isEntrance point model then
        case model.entrance of
          Nothing ->
            Nothing
          Just (pt,open) ->
            Just (Entity.entrance open point)
      else
        Nothing

    crystal =
      if isCrystal point model then
        case model.crystal of
          Nothing ->
            Nothing
          Just (pt,taken) ->
            Just (Entity.crystal taken point)
      else
        Nothing

    entities =
      [ floor
      , door
      , wall
      , coin
      , monster
      , downstairs
      , upstairs
      , entrance
      , crystal
      ]
  in
    entities
    |> List.filterMap identity

creatureAt : Point -> Level -> Maybe Creature.Model
creatureAt pt model =
  let
    creatures' =
      List.filter (\c -> c.position == pt) model.creatures
  in
    List.head creatures'

-- HELPERS (for update)

playerSees : List Point -> Level -> Level
playerSees pts model =
  let
    pts' =
      pts
      |> List.filter (\pt -> not (List.member pt model.viewed))

    viewed' =
      model.viewed
  in
    { model | viewed = viewed' ++ pts' }

moveCreatures : Warrior.Model -> Level -> (Level, List Event, Warrior.Model)
moveCreatures player model =
  model.creatures
  |> List.foldl creatureSteps (model, [], player)

creatureSteps : Creature.Model -> (Level, List Event, Warrior.Model) -> (Level, List Event, Warrior.Model)
creatureSteps creature (model, events, player) =
  let distance = Point.distance player.position creature.position in
  if distance < 5 || creature.engaged then
    (model |> creatureEngages player creature, events, player)
    |> stepCreature creature
  else
    (model, events, player)

stepCreature : Creature.Model -> (Level, List Event, Warrior.Model) -> (Level, List Event, Warrior.Model)
stepCreature creature (model, events, player) =
    ((model |> creatureMoves creature player), events, player)
    |> creatureAttacks creature

canCreatureStep creature player model =
  let
    next =
      creature.position
      |> Point.slide creature.direction

    isPlayer =
      player.position == next

    blocked =
      isPlayer ||
      (model |> isWall next) ||
      (model |> isCreature next)
  in
    not blocked

creatureEngages : Warrior.Model -> Creature.Model -> Level -> Level
creatureEngages warrior creature model =
  let
    creatures' =
      model.creatures
      |> List.map (\c ->
        if c.id == creature.id then
           c
           |> Creature.engage
           |> Creature.turn --direction
               ((Point.towards c.position warrior.position)
               |> Direction.invert)
        else c
      )
  in
    { model | creatures = creatures' }

creatureMoves : Creature.Model -> Warrior.Model -> Level -> Level
creatureMoves creature player model =
  let
    creatures' =
      model.creatures
      |> List.map (\c ->
        if c.id == creature.id && (model |> canCreatureStep c player) then -- model
          c |> Creature.step
        else c)
  in
      { model | creatures = creatures' }

creatureAttacks : Creature.Model -> (Level, List Event, Warrior.Model) -> (Level, List Event, Warrior.Model)
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


injureCreature : Creature.Model -> Int -> Level -> Level
injureCreature creature amount model =
  let
    creatures' =
      model.creatures
      |> List.map (\c ->
        if c.id == creature.id then
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
      |> Set.remove pt -- (not << (\c -> c == pt))
  in
    { model | coins = coins' }

liberateCrystal : Level -> Level
liberateCrystal model =
  { model | crystal = case model.crystal of
      Nothing -> Nothing
      Just (pt,taken) ->
        Just (pt, True)
    }

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
    |> connectRooms rooms
    |> extrudeStairwells
    |> dropCoins

extrudeRooms : Level -> Level
extrudeRooms model =
  model.rooms
  |> List.foldr extrudeRoom model

extrudeRoom : Room -> Level -> Level
extrudeRoom room model =
  let
    (walls,floors) =
      Room.layout room
  in
    { model | walls  = Set.union model.walls walls
            , floors = Set.union model.floors floors }

connectRooms : List Room -> Level -> Level
connectRooms rooms model =
  let
    maybeNetwork =
      Room.network rooms
  in
    case maybeNetwork of
      Just graph ->
        let
          model' =
            ({ model | rooms = Graph.listNodes graph }
            |> extrudeRooms)
        in
          graph
          |> Graph.fold connectRooms' model'

      Nothing ->
        model

connectRooms' : (Room,Room) -> Level -> Level
connectRooms' (a, b) model =
  let
    (ax,ay) =
      a.origin

    (bx,by) =
      b.origin

    direction =
      Direction.invert (Room.directionBetween a b)

    xOverlapStart =
      (max ax bx) + 1

    xOverlapEnd =
      (min (ax+a.width) (bx+b.width)) - 1

    xOverlapRange =
      [(xOverlapStart)..(xOverlapEnd)]

    sampleOverlap = \overlap ->
       Util.getAt overlap ((a.height ^ 31 + bx) % (max 1 (List.length overlap - 1)))
       |> Maybe.withDefault -1

    yOverlapStart =
      (max ay by) + 1

    yOverlapEnd =
      (min (ay+a.height) (by+b.height)) - 1

    yOverlapRange =
      [(yOverlapStart)..(yOverlapEnd)]

    startPosition =
      case direction of
        North ->
          Just ((sampleOverlap xOverlapRange), ay)

        South ->
          Just ((sampleOverlap xOverlapRange), ay+a.height)

        East ->
          Just (ax+a.width, (sampleOverlap yOverlapRange))

        West ->
          Just (ax, (sampleOverlap yOverlapRange))

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
--  extrudeCorridor' pt dir depth model
--extrudeCorridor' pt dir depth model =
  let
    model' =
      { model | floors = Set.insert pt model.floors  }
              |> addWallsAround pt
              |> removeWall pt

    next =
      Point.slide dir pt

    foundFloor =
      model |> isFloor pt
      --(List.any (\pt' -> pt' == pt) model.floors)
  in
    if foundFloor || depth < 0 then
      model'
      |> emplaceDoor (pt |> Point.slide (Direction.invert dir))
    else
      model'
      |> extrudeCorridor (depth-1) (pt |> Point.slide dir) dir


-- doors
emplaceDoor : Point -> Level -> Level
emplaceDoor pt model =
  { model | doors = Set.insert pt model.doors }
          |> removeWall pt

-- stairs

extrudeStairwells : Level -> Level
extrudeStairwells model =
  let
    adjacentToFloor = (\pt ->
        (Direction.cardinalDirections
        |> List.map (\direction -> Point.slide direction pt)
        |> List.filter (\pt' -> Set.member pt' (model.floors))
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
      |> Set.filter adjacentToFloor
      |> Set.filter adjacentToTwoWalls
      |> Set.toList

    (up, down) =
      candidates
      |> List.map2 (,) (List.reverse candidates)
      |> List.filter (\(a,b) -> not (a == b))
      |> List.sortBy (\(a,b) -> Point.distance a b)
      |> List.reverse
      |> List.head
      |> Maybe.withDefault (origin, origin)
  in
    model
    |> emplaceUpstairs up
    |> emplaceDownstairs down

emplaceUpstairs : Point -> Level -> Level
emplaceUpstairs point model =
  { model | upstairs = Just point }
          |> addWallsAround point
          |> removeWall point

emplaceDownstairs : Point -> Level -> Level
emplaceDownstairs point model =
  { model | downstairs = Just point }
          |> addWallsAround point
          |> removeWall point

emplaceCrystal : Point -> Level -> Level
emplaceCrystal point model =
  { model | crystal = Just (point, False)
          , downstairs = Nothing
          }
          |> addWallsAround point
          |> removeWall point

emplaceEntrance : Point -> Level -> Level
emplaceEntrance point model =
  { model | entrance = Just (point, False)
          , upstairs = Nothing
          }
          |> addWallsAround point
          |> removeWall point

removeWall pt model =
  { model | walls = Set.remove pt model.walls }

removeFloor pt model =
  { model | floors = Set.remove pt model.floors }

addWallsAround pt model =
  let
    newWalls =
      Direction.directions
      |> List.map (\d -> Point.slide d pt)
      |> Set.fromList
      |> Set.filter (\wall -> not ( (model |> isFloor wall)))
  in
     { model | walls = Set.union newWalls model.walls }

dropCoins : Level -> Level
dropCoins model =
  let
    down =
      case model.downstairs of
        Just pt ->
          pt
        Nothing ->
          case model.crystal of
            Just (pt,_) ->
              pt
            Nothing ->
              origin

    up =
       case model.upstairs of
        Just pt ->
          pt
        Nothing ->
          case model.entrance of
            Just (pt,_) ->
              pt
            Nothing ->
              origin

    path' =
      Path.seek up down (not << (\pt -> isWall pt model))
      |> List.tail |> Maybe.withDefault []
      |> List.reverse
      |> List.tail |> Maybe.withDefault []

    everyN = \n ls ->
      case (ls |> List.drop (n-1)) of
        [] ->
          []
        (head :: rest) ->
          head :: (everyN n rest)

    coins' =
      path' |> everyN (4) --List.length path' // 3)
  in

    { model | coins = Set.fromList coins' }

spawnCreatures : Int -> Level -> Level
spawnCreatures depth model =
  let
    creature =
      creatureForDepth depth

    creatures' =
      model.rooms
      |> List.map Room.center
      |> List.indexedMap (\n pt -> creature n pt)
  in
    { model | creatures = creatures' }

creatureForDepth : Int -> (Int -> Point -> Creature.Model)
creatureForDepth depth =
  if depth < 3 then
     Creature.createRat
  else
    if depth < 6 then
      Creature.createMonkey
    else
      Creature.createBandit


-- pathfinding
--path : Point -> Point -> (Point -> Bool) -> Maybe (List Point)
--path dst src blocked =
--  Path.find dst src (movesFrom blocked)
--
--movesFrom : (Point -> Bool) ->  Point -> List (Point, Direction)
--movesFrom blocked point =
--  Direction.directions
--  |> List.map (\direction -> (Point.slide direction point, direction))
--  |> List.filter ((\p -> not (blocked p)) << fst)
