module World exposing (Model, Msg, init, update, view, playerSteps, creatureSteps, moveCreaturesCommand)

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
  , coins = [{x=4,y=3}, {x=9,y=8}, {x=3,y=7}, {x=8,y=3}]
  , creatures =
      [ Creature.createRat 1 {x=3,y=3}
      , Creature.createMonkey 2 {x=6,y=9}
      ]
  , player = Warrior.init
  , events = Log.init
  }

-- just a little four walled room for now...
assembleWalls =
  List.map (\x -> {x=x,y=2}) [0..10] ++
  List.map (\x -> {x=x,y=10}) [0..10] ++
  List.map (\y -> {x=0,y=y}) [2..10] ++
  List.map (\y -> {x=10,y=y}) [2..10]

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

isAlive livingThing =
  livingThing.hp > 0

-- UPDATE
type Msg = MoveCreature Int Direction

update : Msg -> Model -> Model
update message model =
  case message of
    MoveCreature id direction ->
      creatureSteps id direction model

-- command ctors
moveCreaturesCommand : Model -> (Msg -> superMsgType) -> Cmd superMsgType
moveCreaturesCommand model superMsg =
  Cmd.batch
    ( --List.map (\msg -> (superMsg msg))
      (List.map (\creature -> moveCreatureRandomly superMsg creature) model.creatures)
    )

--moveCreatureRandom : superMsgType -> Rogue.Creature.Model -> Msg
moveCreatureRandomly superMsg creature =
  Random.generate (\dir -> superMsg (MoveCreature creature.id dir)) Direction.random


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
        List.filter (\coin -> not (coin == model.player.position)) model.coins

      collectionEvent =
        Event.pickupCoinEvent

      player =
        model.player
    in
      { model | player = { player | gold = player.gold + 1 }
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
      Event.attackEvent creature amount
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
      List.map Event.killEnemyEvent killed
  in
    { model | creatures = survivors
            , events = model.events ++ deathEvents
    }

-- CREATURE MOVE
creatureSteps id direction model =
  model
  |> creatureMoves id direction
  |> creatureAttacks id direction

creatureMoves id direction model =
  let
    creatures =
      List.map (moveIndicatedCreature id direction model) model.creatures
  in
    { model | creatures = creatures }

moveIndicatedCreature id direction model creature =
  let
    isIndicated =
      creature.id == id

    shouldMove =
      canCreatureStep creature direction model
  in
    if (isIndicated && shouldMove) then
      { creature | position = slide creature.position direction }
    else
      creature

canCreatureStep : Creature.Model -> Direction -> Model -> Bool
canCreatureStep creature direction model =
  let
    nextPosition =
      slide creature.position direction
  in
    not (isBlocked model nextPosition)

creatureAttacks : Int -> Direction -> Model -> Model
creatureAttacks id direction model =
  let
    maybeCreature =
      creatureById model.creatures id
  in
    case maybeCreature of
      Nothing ->
        model

      Just creature ->
        model
        |> creatureAttacksPlayer creature direction

creatureAttacksPlayer : Creature.Model -> Direction -> Model -> Model
creatureAttacksPlayer creature direction model =
  let
    attackedPosition =
      slide creature.position direction

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
      model.player

    defenseEvent =
      Event.defendEvent creature amount
  in
    { model |
      player = { player | hp = player.hp - amount }
    , events = model.events ++ [defenseEvent]
    }

checkPlayerLife : Model -> Model
checkPlayerLife model =
  if not (isAlive model.player) then
    -- TODO shift to game over state?
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
