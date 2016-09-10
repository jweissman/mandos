module Level exposing (Level, init, fromRooms, finalize, moveCreatures, injureCreature, purge, collectCoin, isCoin, isEntrance, isCreature, creatureAt, entitiesAt, playerSees, itemAt, removeItem, crystalLocation, extrude, evolveGrass)


import Point exposing (Point)
import Direction exposing (Direction(..))
import Room exposing (Room, Purpose(..))
import Graph exposing (Graph)
import Util
import Path
import Configuration
import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Helm exposing (Helm)
import Ring exposing (Ring)
import Warrior
import Creature
import Species
import Liquid
import Spell
import ChallengeRating
import Event exposing (Event)
import Entity exposing (Entity)
import Item exposing (Item)
import Set exposing (Set)

-- TYPE

type alias Level = { walls : Set Point
                   , floors : Set Point
                   , doors : Set Point
                   , coins : Set Point
                   , grass : Set Point

                   , creatures : List Creature.Model
                   , downstairs : Maybe Point
                   , upstairs : Maybe Point
                   , entrance : Maybe (Point, Bool)

                   , items : List Item

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
  , grass = Set.empty
  , creatures = []
  , upstairs = Nothing
  , downstairs = Nothing
  , entrance = Nothing
  , rooms = []
  , viewed = []
  , items = []
  }

finalize : Int -> Level -> Level
finalize depth model =
  model
  |> finalizeEntrance depth
  |> finalizeCrystal depth
  |> furnishRooms depth
  |> spawnCreatures depth

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

isGrass : Point -> Level -> Bool
isGrass pt model =
  Set.member pt model.grass

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

isEntrance : Point -> Level -> Bool
isEntrance position model =
  case model.entrance of
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

    grass =
      if isGrass point model then
        Just (Entity.grass point)
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

    item =
      case itemAt point model of
        Just item' ->
          Just (Entity.item item')

        Nothing ->
          Nothing

    entities =
      [ floor
      , door
      , wall
      , coin
      , downstairs
      , upstairs
      , entrance
      , grass
      , item
      , monster
      ]
  in
    entities
    |> List.filterMap identity

creatureAt : Point -> Level -> Maybe Creature.Model
creatureAt pt model =
  model.creatures
  |> List.filter (\c -> c.position == pt)
  |> List.head

itemAt : Point -> Level -> Maybe Item
itemAt pt model =
  model.items
  |> List.filter (\item -> pt == (item.position))
  |> List.head

crystalLocation : Level -> Maybe Point
crystalLocation model =
  model.items
  |> List.filter (\{kind} -> kind == Item.crystal)
  |> List.map .position
  |> List.head

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
           |> Creature.turn
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
        if c.id == creature.id && (model |> canCreatureStep c player) then
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
      max 1 (creature.attack - (Warrior.resistance player))
  in
    if pos == player.position then
       (model, events, player)
       |> playerTakesDamage creature dmg
       |> playerDies ("killed by " ++ (Creature.describe creature))
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

playerDies cause (model, events, player) =
  if not (isAlive player) && not (List.any Event.isPlayerDeath events) then
    let event = Event.death cause in
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
      model.coins |> Set.remove pt
  in
    { model | coins = coins' }

removeItem : Item -> Level -> Level
removeItem item model =
  let
    items' =
      model.items
      |> List.filter (\it -> not (it == item))
  in
    { model | items = items' }

-- GENERATE

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
    |> growGrass

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
    corridor =
      Room.corridor a b
  in
    case corridor of
      [] -> model
      [pt] ->
        model
        |> emplaceDoor pt
      _ ->
        if List.length corridor > 2 then
          model
          |> extrudeCorridor corridor
          |> emplaceDoor (corridor |> List.head |> Maybe.withDefault origin)
          |> emplaceDoor (corridor |> List.reverse |> List.head |> Maybe.withDefault origin)
        else
          model
          |> extrudeCorridor corridor

extrudeCorridor : List Point -> Level -> Level
extrudeCorridor pts model =
  pts
  |> List.foldr extrude model

--extrudeCorridor' pt model =
--  { model | floors = Set.insert pt model.floors  }
--          |> addWallsAround pt
--          |> removeWall pt

extrude pt model =
  model
  |> addFloor pt
  |> addWallsAround pt
  |> removeWall pt

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
  let crystal = Item.init point Item.crystal -1 in
  { model | items = model.items ++ [ crystal ]
          , downstairs = Nothing
          }
          |> addWallsAround point
          |> removeWall point
          |> addFloor point

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

addFloor pt model =
  { model | floors = Set.insert pt model.floors }

addWallsAround pt model =
  let
    newWalls =
      Direction.directions
      |> List.map (\d -> Point.slide d pt)
      |> Set.fromList
      |> Set.filter (\wall -> not ( (model |> isFloor wall) || (model |> isDoor wall)))
  in
     { model | walls = Set.union newWalls model.walls }

dropCoins : Level -> Level
dropCoins model =
  let
    path' =
      model
      |> bestPath
      |> List.tail |> Maybe.withDefault []
      |> List.reverse
      |> List.tail |> Maybe.withDefault []

    coins' =
      path'
      |> Util.everyNth 8
      |> List.filter (\pt -> not (isDoor pt model))
      |> Set.fromList
  in
    { model | coins = coins' }

furnishRooms : Int -> Level -> Level
furnishRooms depth model =
  let model' = model |> assignRooms depth in
  model'.rooms
  |> List.foldr (furnishRoom depth) model'

assignRooms : Int -> Level -> Level
assignRooms depth model =
  let
    rooms' =
      model.rooms
      |> Util.mapEveryNth 3 (Room.assign Room.library)
      |> Util.mapEveryNth 4 (Room.assign Room.barracks)
      |> Util.mapEveryNth 5 (Room.assign Room.armory)
      |> Util.mapEveryNth 6 (Room.assign Room.miningCamp)
      |> List.indexedMap (\id room -> { room | id = id })
  in
    { model | rooms = rooms' }

furnishRoom : Int -> Room -> Level -> Level
furnishRoom depth room model =
  case room.purpose of
    Nothing ->
      model

    Just purpose ->
      model
      |> furnishRoomFor purpose room depth

furnishRoomFor : Purpose -> Room -> Int -> Level -> Level
furnishRoomFor purpose room depth model =
  let
    itemKinds =
      case purpose of
        Armory ->
          [ Item.helm Helm.cap
          , Item.scroll Spell.infuse
          , Item.bottle Liquid.water
          , Item.weapon Weapon.dagger
          , Item.armor Armor.suit
          , Item.weapon Weapon.whip
          ]

        Barracks ->
          [ Item.helm Helm.helmet
          , Item.scroll Spell.infuse
          , Item.weapon Weapon.sword
          , Item.bottle Liquid.water
          , Item.weapon Weapon.axe
          , Item.scroll Spell.lux
          , Item.armor Armor.plate
          ]

        Library ->
          [ Item.ring Ring.light
          , Item.scroll Spell.infuse
          , Item.bottle Liquid.water
          , Item.scroll Spell.lux
          , Item.armor Armor.tunic
          , Item.bottle Liquid.lifePotion
          ]

        MiningCamp ->
          [ Item.ring Ring.power
          , Item.weapon Weapon.pick
          , Item.bottle Liquid.water
          , Item.scroll Spell.lux
          , Item.bottle Liquid.lifePotion
          , Item.weapon Weapon.whip
          ]
    idRange =
      [(depth*10000)+(room.id*100)..(depth)*10000+((room.id+1)*100)]

    items =
      List.map3 (\pt idx kind -> Item.init pt kind idx) targets idRange itemKinds

    (_,floors') =
      Room.layout room

    targets =
      floors'
      |> Set.toList
      |> Util.everyNth 17
  in
    furnishRoomWith items room model

furnishRoomWith : List Item -> Room -> Level -> Level
furnishRoomWith items room model =
  items
  |>  List.foldr (furnishRoomWith' room) model

furnishRoomWith' : Room -> Item -> Level -> Level
furnishRoomWith' room item model =
  model |> addItem item

addItem : Item -> Level -> Level
addItem item model =
  { model | items = item :: model.items }

spawnCreatures : Int -> Level -> Level
spawnCreatures depth model =
  model.rooms
  |> List.foldr (spawnCreaturesForRoom depth) model

spawnCreaturesForRoom : Int -> Room -> Level -> Level
spawnCreaturesForRoom depth room model =
  let
    species =
      Species.level (ChallengeRating.forDepth depth)

    (_,floors) =
      Room.layout room

    spawnTargets =
      floors
      |> Set.toList
      |> Util.everyNth 9

    creatureCount =
      1 + (List.length (model.creatures))

    spawnCount =
      1 + (creatureCount % 5)

    creatures' =
      spawnTargets
      |> List.map3 (\species n pt -> Creature.init species n pt) species [(creatureCount)..(creatureCount+spawnCount)]
  in
    { model | creatures = model.creatures ++ creatures' }

bestPath : Level -> List Point
bestPath model =
  let
    down =
      case model.downstairs of
        Just pt ->
          pt

        Nothing ->
          case (crystalLocation model) of
            Just pt ->
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
  in
    Path.seek up down (\pt -> isWall pt model)

growGrass : Level -> Level
growGrass model =
  let
    {floors, walls} =
      model

    seeds =
      floors
      |> Set.toList
      |> Util.filterChamp
  in
    seeds
    |> List.foldr seedGrassAt model
    |> evolveGrass 25

seedGrassAt : Point -> Level -> Level
seedGrassAt pt model =
  let
    grass' =
      Set.insert pt model.grass
  in
     { model | grass = grass' }

removeGrassAt : Point -> Level -> Level
removeGrassAt pt model =
  let
    grass' =
      Set.remove pt model.grass
  in
     { model | grass = grass' }

evolveGrass : Int -> Level -> Level
evolveGrass n model =
  if n < 0 then
    model
  else
    let
      (add,remove) =
        model.floors
        |> Set.toList
        |> List.foldr (evolveGrassAt model) ([],[])

      model' =
        (add |> List.foldr seedGrassAt model)
    in
      (remove |> List.foldr removeGrassAt model')

evolveGrassAt model pt (add,remove) =
  let
    neighbors =
      Direction.directions
      |> List.map (\dir -> Point.slide dir pt)
      |> List.filter (\pt -> model |> isGrass pt)
      |> List.length

    alive =
      Set.member pt model.grass
  in
    if alive then
      if neighbors < 3 || 5 < neighbors then
        (add, pt :: remove)
      else
        (add, remove)
    else if neighbors > 2 && 6 > neighbors then
      (pt :: add, remove)
    else
      (add, remove)
