module World exposing (Model, Msg, init, update, view, playerSteps, creatureSteps, turnCreaturesCommand, moveCreatures)

import Warrior exposing (Model)
import Creature exposing (Model, view, createRat)

import Point exposing (Point, slide)
import Direction exposing (Direction)

import Log
import Event exposing (..)

import Html
import Svg exposing (text')
import Svg.Attributes exposing (x,y,fontSize,fontFamily)

import Random

-- MODEL

type alias Model =
  { walls : List Point
  , coins : List Point
  , creatures : List Creature.Model
  , player : Warrior.Model
  , events : Log.Model
  }

-- INIT
init : Model
init =
  { walls = assembleWalls
  , coins = [{x=4,y=3}, {x=9,y=8}, {x=3,y=7}, {x=8,y=3}, {x=9,y=7}]
  , creatures =
      [ Creature.createRat 1 {x=3,y=3}
      , Creature.createMonkey 2 {x=6,y=9}
      , Creature.createBandit 3 {x=5,y=6}
      ]
  , player = Warrior.init
  , events = Log.init
  }

-- just a little four walled room for now...
assembleWalls =
  List.map (\x -> {x=x,y=2}) [0..10] ++
  List.map (\x -> {x=x,y=20}) [0..10] ++
  List.map (\y -> {x=0,y=y}) [2..20] ++
  List.map (\y -> {x=10,y=y}) [2..20]

-- HELPER

isWall : Model -> Point -> Bool
isWall model position =
  List.any (\pos -> pos == position) model.walls

isCoin : Model -> Point -> Bool
isCoin model position =
  List.any (\pos -> pos == position) model.coins

isCreature : Model -> Point -> Bool
isCreature model position =
  List.any (\pos -> pos == position) (List.map .position model.creatures)

isPlayer : Model -> Point -> Bool
isPlayer model position =
  model.player.position == position

isBlocked : Model -> Point -> Bool
isBlocked model position =
  isWall model position ||
  isCreature model position ||
  isPlayer model position

--isAlive : { hp : Int } -> Bool
isAlive livingThing =
  livingThing.hp > 0

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
      slide model.player.position direction
  in
    not (isBlocked model nextPosition)

playerCollectsCoins : Model -> Model
playerCollectsCoins model =
  if (not (isCoin model model.player.position)) then
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
      slide model.player.position direction

    maybeCreature =
      creatureAt model.creatures attackedPosition
  in
    case maybeCreature of
      Nothing ->
        model

      Just creature ->
        model
        |> playerAttacksCreature creature
        |> removeDeceasedCreatures

creatureAt : List Creature.Model -> Point -> Maybe Creature.Model
creatureAt creatures position =
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
  in
    model
    |> creatureTakesDamage creature damage

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

-- CREATURE MOVE
creatureTurns : Int -> Direction -> Model -> Model
creatureTurns id direction model =
  let
    creatures =
      model.creatures
      |> List.map (turnIndicatedCreature id direction)
  in
    { model | creatures = creatures }

turnIndicatedCreature : Int -> Direction -> Creature.Model -> Creature.Model
turnIndicatedCreature id direction creature =
  let
    isIndicated =
      creature.id == id
  in
    if isIndicated then
      Creature.turn direction creature
    else
      creature

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
      slide creature.position direction
  in
    not (isBlocked model nextPosition)

creatureAttacksPlayer : Creature.Model -> Model -> Model
creatureAttacksPlayer creature model =
  let
    attackedPosition =
      slide creature.position creature.direction

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
    { model |
      player = player
    , events = model.events ++ [defenseEvent]
    }

checkPlayerLife : Model -> Model
checkPlayerLife model =
  if not (isAlive model.player) then
    init
  else
    model

-- VIEW
view : Model -> List (Svg.Svg a)
view model =
  let
    walls =
      List.map wallView model.walls

    coins =
      List.map coinView model.coins

    creatures =
      List.map Creature.view model.creatures

    player =
      Warrior.view model.player

    log =
      Log.view model.events

    info =
      infoView model
  in
    walls ++ coins ++ creatures ++ log ++ [player, info]

wallView : Point -> Svg.Svg a
wallView pos =
  text' [ x (toString pos.x), y (toString pos.y), fontSize "1", fontFamily "Courier" ] [ Html.text "#" ]

coinView : Point -> Svg.Svg a
coinView pos =
  text' [ x (toString pos.x), y (toString pos.y), fontSize "1", fontFamily "Courier" ] [ Html.text "." ]

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
    text' [ x "0", y "1", fontSize "1", fontFamily "Courier" ] [ Html.text message ]
