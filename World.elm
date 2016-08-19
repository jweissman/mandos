module World exposing (Model, Msg, init, update, view, playerSteps, creatureSteps, turnCreaturesCommand, moveCreatures, isWall, isCoin, isPlayer, isBlocked, entityAt)

import Point exposing (Point, slide)
import Direction exposing (Direction)

import Warrior
import Creature
import Entity exposing (Entity)
import Room exposing (Room)

import Log
import Event exposing (..)

import Util

import Html
import Graphics
import Svg

import Random

-- MODEL

type alias Model =
  { walls : List Point
  , floors : List Point
  , coins : List Point
  --, rooms : List Room
  , creatures : List Creature.Model
  , player : Warrior.Model
  , events : Log.Model
  , debugPath : List Point
  }

-- INIT
init : Model
init =
  let
    (walls,floors) =
      layoutRoom {x=2,y=3} 30 20

    coins =
      [ {x=4,y=10}
      , {x=9,y=8}
      , {x=5,y=7}
      , {x=8,y=8}
      , {x=9,y=7}
      , {x=29,y=17}
      , {x=15,y=8}
      , {x=23,y=13}
      ]
  in
    { walls = walls
    , floors = floors
    , coins = coins
    , creatures =
        [ 
          Creature.createRat 1 {x=10,y=8}
        --, Creature.createMonkey 2 {x=16,y=9}
        --, Creature.createBandit 3 {x=15,y=6}
        ]
    , player = Warrior.init {x=10,y=10}
    , events = Log.init
    , debugPath = []
    --, rooms = []
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
  in
    (walls,floors)


-- PREDICATES

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

-- query

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
          case creature of
            Just creature ->
              Just (Entity.monster creature)
            Nothing ->
              if isFloor point world then
                Just (Entity.floor point)
              else
                Nothing

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

-- UPDATE
type Msg = TurnCreature Int Direction

update : Msg -> Model -> Model
update message model =
  case message of
    TurnCreature id direction ->
      model
      |> creatureTurns id direction

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

playerAttacksCreature : Creature.Model -> Model -> Model
playerAttacksCreature creature model =
  let
    damage =
      model.player.attack - creature.defense
  in
    model
    |> creatureTakesDamage creature damage
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

turnIndicatedCreature : Int -> Direction -> Model -> Creature.Model -> Creature.Model
turnIndicatedCreature id direction model creature =
  let
    isIndicated =
      creature.id == id
  in
    if isIndicated then
      if creature.engaged then
        let
          followDirection =
            (Util.directionBetween model.player.position creature.position)
        in
          Creature.turn followDirection creature
      else
        Creature.turn direction creature
    else
      creature

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
      |> playerDies
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

playerDies : Model -> Model
playerDies model =
  if not (isAlive model.player) then
    init
  else
    model

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

highlightCells : List Point -> List (Svg.Svg a)
highlightCells cells =
  let
    pathColor =
      "rgba(128,128,192,0.85)"
    targetColor =
      "rgba(128,128,192,0.3)"
  in

    case cells of
      [] -> []
      [x] -> [highlightCell x pathColor]
      a :: b :: _ ->
        let
          tail =
            case (List.tail cells) of
              Nothing -> []
              Just rest -> highlightCells rest
        in
          (highlightCell a targetColor) :: tail

highlightCell {x,y} color =
  Graphics.render "@" {x=x,y=y} color


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
