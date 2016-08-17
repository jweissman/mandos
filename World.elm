module World exposing (Model, Msg, init, update, view, playerSteps, creatureSteps, turnCreaturesCommand, moveCreatures, isWall, isCoin, isPlayer, isBlocked, entityAt, bfs) --, toGraph)

import Point exposing (Point, slide)
import Direction exposing (Direction)

import Warrior --exposing (Model)
import Creature --exposing (Model, createRat)
import Entity exposing (Entity)

import Log
import Event exposing (..)

import Util

--import Graph

import Set exposing (Set)

import Html
import Graphics
import Svg

import Random

-- MODEL

type alias Model =
  { walls : List Point
  , floors : List Point
  , coins : List Point
  , creatures : List Creature.Model
  , player : Warrior.Model
  , events : Log.Model
  , debugPath : List Point
  }

-- INIT
init : Model
init =
  let
    (walls,floors) = layoutRoom {x=2,y=3} 30 20
  in
    { walls = walls --assembleWalls {x=2,y=3} 20 20
    , floors = floors
    , coins = [{x=4,y=10}, {x=9,y=8}, {x=5,y=7}, {x=8,y=8}, {x=9,y=7}]
    , creatures =
        [ Creature.createRat 1 {x=10,y=8}
        , Creature.createMonkey 2 {x=16,y=9}
        , Creature.createBandit 3 {x=15,y=6}
        ]
    , player = Warrior.init {x=10,y=10}
    , events = Log.init
    , debugPath = []
    }

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
      --List.map2 (\x' y' -> {x=x+x', y=y+y'}) [1..(width-1)] [1..(height-1)]
  in
    (walls,floors)


-- HELPER

type alias Path = List Point

bfs : Point -> (Point -> Bool) -> Model -> Maybe Path
bfs source predicate model =
  bfs' [] [] source predicate (movesFrom model) 100 model

movesFrom : Model -> Point -> List (Point, Direction)
movesFrom model point =
  Direction.directions
  |> List.map (\direction -> (slide direction point, direction))
  |> List.filter (\(p,_) -> not (isBlocked p model))

bfs' : List (Point, Direction) -> List (Point, Direction) -> Point -> (Point -> Bool) -> (Point -> List (Point, Direction)) -> Int -> Model -> Maybe Path
bfs' visited frontier source predicate moves depth model =
  if depth < 0 then
    Nothing
  else
    let
      matches =
        \(v,_) -> predicate v

      maybeGoal =
        frontier
        |> List.filter matches
        |> List.head
    in
      case maybeGoal of
        Just (goal,_) ->
          --Debug.log "FOUND PATH!"
          Just (List.reverse (constructPath (visited ++ frontier) source goal))

        Nothing ->
          if List.length frontier == 0 then
            let
              availableToVisit =
                moves source
            in
              bfs' visited availableToVisit source predicate moves (depth-1) model
          else
            let
              pointCode =
                (\({x,y},_) -> (x*100) + y)

              visitedPositions =
                List.map fst newVisited

              newFrontier =
                frontier
                |> List.concatMap (\(pt,_) -> moves pt)
                |> List.filter (\(pt,_) -> not (List.member pt visitedPositions)) --moves pt)
                |> uniqueBy pointCode

              newVisited =
                --uniqueBy pointCode 
                (visited ++ frontier)
            in
              if List.length frontier > 0 then
                --Debug.log ("bfs at depth: " ++ (toString (100-depth)))
                --Debug.log ("visited "++ (toString (List.length newVisited)) ++ ": " ++ (toString (List.map fst newVisited)))
                bfs' newVisited (newFrontier) source predicate moves (depth-1) model
              else
                Nothing

-- from list extras
uniqueBy f list =
  uniqueHelp f Set.empty list

uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a 
uniqueHelp f existing remaining =
  case remaining of
    [] ->
      []

    first :: rest ->
      let computedFirst = f first in
      if Set.member computedFirst existing then
        uniqueHelp f existing rest
      else
        first :: uniqueHelp f (Set.insert computedFirst existing) rest

constructPath : List (Point, Direction) -> Point -> Point -> Path
constructPath visited source destination =
  let
    maybeDestination =
      visited
      |> List.filter (\(pt,_) -> pt == destination)
      |> List.head
  in
     if source == destination then
        []
     else
       case maybeDestination of
         Nothing ->
           []

         Just (point, direction) ->
           let
             newDest =
               point
               |> slide (Direction.invert direction)
           in
             --Debug.log "constructPath"
             [destination] ++ (constructPath visited source newDest)

---

isWall : Point -> Model -> Bool
isWall position model =
  List.any (\pos -> pos == position) model.walls

isCoin : Point -> Model -> Bool
isCoin position model =
  List.any (\pos -> pos == position) model.coins

isCreature : Point -> Model -> Bool
isCreature position model =
  List.any (\pos -> pos == position) (List.map .position model.creatures)

isPlayer : Point -> Model -> Bool
isPlayer position model =
  model.player.position == position

isFloor : Point -> Model -> Bool
isFloor position model =
  List.any (\p -> p == position) model.floors

isBlocked : Point -> Model -> Bool
isBlocked position model =
  isWall position model ||
  isCreature position model ||
  isPlayer position model

isAlive livingThing =
  livingThing.hp > 0

entityAt : Point -> Model -> Maybe Entity
entityAt point world =
  if isWall point world then
    Just (Entity.wall point)
  else
    if isCoin point world then
      Just (Entity.coin point)
    else
      if isPlayer point world then
        Just (Entity.player world.player)
      else
        let
          creature =
            creatureAt point world
        in
          -- seems like the 'right' place to use Maybe.withDefault?
          case creature of
            Just creature ->
              Just (Entity.monster creature)
            Nothing ->
              if isFloor point world then
                Just (Entity.floor point)
              else
                Nothing

-- UPDATE
type Msg = TurnCreature Int Direction

update : Msg -> Model -> Model
update message model =
  case message of
    TurnCreature id direction ->
      model
      |> creatureTurns id direction
      --creatureSteps id direction model

-- command ctors
turnCreaturesCommand : Model -> (Msg -> a) -> Cmd a
turnCreaturesCommand model superMsg =
  let
    commands =
      model.creatures
      |> List.map (\creature -> turnCreatureRandomly superMsg creature)
  in
    Cmd.batch commands

turnCreatureRandomly : (Msg -> a) -> Creature.Model -> Cmd a
turnCreatureRandomly superMsg creature =
  Random.generate (\dir -> superMsg (TurnCreature creature.id dir)) Direction.random

-- PLAYER STEP
--playerExplores : Model -> Model
--playerExplores model =

playerSteps : Direction -> Model -> Model
playerSteps direction model =
  if not (canPlayerStep direction model) then
    model
    |> playerAttacks direction
  else
    model
    |> playerMoves direction
    |> playerCollectsCoins

playerMoves : Direction -> Model -> Model
playerMoves direction model =
  { model | player = (Warrior.step direction model.player) }

canPlayerStep : Direction -> Model -> Bool
canPlayerStep direction model =
  let
    nextPosition =
      model.player.position
      |> slide direction
  in
    not (isBlocked nextPosition model)

playerCollectsCoins : Model -> Model
playerCollectsCoins model =
  if (not (isCoin model.player.position model)) then
    model
  else
    let
      coinsWithoutCollectedCoin =
        model.coins
        |> List.filter (\coin -> not (coin == model.player.position))

      collectionEvent =
        Event.pickupCoin
    in
      { model | player = Warrior.enrich 1 model.player
              , coins  = coinsWithoutCollectedCoin
              , events = model.events ++ [collectionEvent]
      }

playerAttacks : Direction -> Model -> Model
playerAttacks direction model =
  let
    attackedPosition =
      model.player.position
      |> slide direction

    maybeCreature =
      creatureAt attackedPosition model
  in
    case maybeCreature of
      Nothing ->
        model

      Just creature ->
        model
        |> playerAttacksCreature creature
        |> removeDeceasedCreatures

creatureAt : Point -> Model -> Maybe Creature.Model
creatureAt position {creatures} =
  let
    creaturesAtPosition =
      List.filter (\c -> c.position == position) creatures
  in
    List.head creaturesAtPosition

creatureById : List Creature.Model -> Int -> Maybe Creature.Model
creatureById creatures id =
  let
    creaturesWithId =
      List.filter (\c -> c.id == id) creatures
  in
    List.head creaturesWithId

playerAttacksCreature : Creature.Model -> Model -> Model
playerAttacksCreature creature model =
  let
    damage =
      model.player.attack - creature.defense

    --inverseDirection =
    --  Direction.invert model.player.direction
  in
    model
    |> creatureTakesDamage creature damage
    --|> creatureTurns creature.id inverseDirection
    |> creatureBecomesEngaged creature

creatureTakesDamage : Creature.Model -> Int -> Model -> Model
creatureTakesDamage creature amount model =
  let
    creatures =
      List.map (damageIndicatedCreature creature.id amount) model.creatures

    attackEvent =
      Event.attack creature amount
  in
    { model | creatures = creatures
            , events = model.events ++ [attackEvent]
    }

damageIndicatedCreature : Int -> Int -> Creature.Model -> Creature.Model
damageIndicatedCreature id amount creature =
  let
    isIndicated =
      creature.id == id
  in
    if isIndicated then
      { creature | hp = creature.hp - amount }
    else
      creature

removeDeceasedCreatures : Model -> Model
removeDeceasedCreatures model =
  let
    survivors =
      List.filter isAlive model.creatures

    killed =
      List.filter (\c -> not (isAlive c)) model.creatures

    deathEvents =
      List.map Event.killEnemy killed
  in
    { model | creatures = survivors
            , events = model.events ++ deathEvents
    }

creatureTurns : Int -> Direction -> Model -> Model
creatureTurns id direction model =
  let
    creatures =
      model.creatures
      |> List.map (turnIndicatedCreature id direction model)
  in
    { model | creatures = creatures }

-- Creature/Util.elm (have ctx on both creature and collabs)
turnIndicatedCreature : Int -> Direction -> Model -> Creature.Model -> Creature.Model
turnIndicatedCreature id direction model creature =
  let
    isIndicated =
      creature.id == id
  in
    if isIndicated then
      if creature.engaged then
        -- need direction to first step of path to target
        -- k&& not (creature.engaged) then
        creature
        |> followPlayer model
      else
        Creature.turn direction creature
    else
      creature

followPlayer : Model -> Creature.Model -> Creature.Model
followPlayer model creature =
  creature
  |> Creature.turn (Util.directionBetween model.player.position creature.position)

creatureBecomesEngaged : Creature.Model -> Model -> Model
creatureBecomesEngaged creature model =
  let
    creatures =
      model.creatures
      |> List.map (engageIndicatedCreature creature.id)
  in
    { model | creatures = creatures }

engageIndicatedCreature : Int -> Creature.Model -> Creature.Model
engageIndicatedCreature id creature =
  let
    isIndicated =
      creature.id == id
  in
    if isIndicated then
      Creature.engage creature
    else
      creature

-- CREATURE MOVE
moveCreatures : Model -> Model
moveCreatures model =
  List.foldl (creatureSteps) model model.creatures

creatureSteps : Creature.Model -> Model -> Model
creatureSteps creature model =
  model
  |> creatureMoves creature
  |> creatureAttacksPlayer creature

creatureMoves : Creature.Model -> Model -> Model
creatureMoves creature model =
  let
    creatures =
      List.map (moveIndicatedCreature creature.id model) model.creatures
  in
    { model | creatures = creatures }

moveIndicatedCreature : Int -> Model -> Creature.Model -> Creature.Model
moveIndicatedCreature id model creature =
  let
    isIndicated =
      creature.id == id

    shouldMove =
      canCreatureStep creature creature.direction model
  in
    if isIndicated && shouldMove then
      Creature.step creature
    else
      creature

canCreatureStep : Creature.Model -> Direction -> Model -> Bool
canCreatureStep creature direction model =
  let
    nextPosition =
      creature.position
      |> slide direction
  in
    not (isBlocked nextPosition model)

creatureAttacksPlayer : Creature.Model -> Model -> Model
creatureAttacksPlayer creature model =
  let
    attackedPosition =
      creature.position
      |> slide creature.direction

    damage =
      creature.attack - model.player.defense
  in
    if attackedPosition == model.player.position then
      model
      |> playerTakesDamage creature damage
      |> checkPlayerLife
    else
      model

playerTakesDamage : Creature.Model -> Int -> Model -> Model
playerTakesDamage creature amount model =
  let
    player =
      (Warrior.takeDamage amount model.player)

    defenseEvent =
      Event.defend creature amount
  in
    { model | player = player
            , events = model.events ++ [defenseEvent]
    }

checkPlayerLife : Model -> Model
checkPlayerLife model =
  if not (isAlive model.player) then
    init
  else
    model


-- more helpers (really for view...)
listEntities : Model -> List Entity
listEntities model =
  let
    walls =
      List.map (\pt -> Entity.wall pt) model.walls

    floors =
      List.map (\pt -> Entity.floor pt) model.floors

    coins =
      List.map (\pt -> Entity.coin pt) model.coins

    creatures =
      List.map (\c -> Entity.monster c) model.creatures

    player =
      Entity.player model.player
  in
    walls ++ floors ++ coins ++ creatures ++ [player]

-- VIEW
view : Model -> List (Svg.Svg a)
view model =
  let
    entities =
      listEntities model

    entityViews =
      List.map (Entity.view) entities

    log =
      Log.view model.events

    info =
      infoView model

    highlight =
      highlightCells model.debugPath
  in
    entityViews ++ log ++ [info] ++ highlight

infoView : Model -> Svg.Svg a
infoView model =
  let
    gold =
      "GOLD: " ++ toString model.player.gold

    hp =
      "HP: " ++ toString model.player.hp ++ "/" ++ toString model.player.maxHp

    message =
      gold ++ "  |  " ++ hp
  in
     Graphics.render message {x=0,y=1} "green"

highlightCells cells =
  List.map highlightCell cells

highlightCell {x,y} =
  Graphics.render "*" {x=x,y=y} "rgba(192,192,192,0.8)"
