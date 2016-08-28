module World exposing (Model, init, view, playerSteps, floors, walls, coins, downstairs, upstairs, entrances, crystals, exploration, playerViewsField, entitiesAt, viewed, canPlayerStep, creatures)

import Point exposing (Point, slide)
import Direction exposing (Direction)

--import Bresenham
import Optics

import Warrior
import Creature
import Entity exposing (Entity)
import Room exposing (Room)

import Dungeon exposing (Dungeon)

import Level exposing (Level)
import Path

import Log
import Event exposing (..)

import Util
import Configuration

import String
import Html
import Graphics
import Svg

import Random

type alias Model =
  {
    depth : Int
  , dungeon : Dungeon
  , player : Warrior.Model
  , events : Log.Model
  , debugPath : List Point
  , illuminated : List Point
  , crystalTaken : Bool
  , hallsEscaped : Bool
  , showMap : Bool
  }

-- INIT
init : Model
init =
  {
    dungeon = []
  , depth = 0
  , player = Warrior.init {x=0,y=0}
  , events = Log.init
  , debugPath = []
  , illuminated = []
  , crystalTaken = False
  , hallsEscaped = False
  , showMap = False
  }


level : Model -> Level
level model =
  Util.getAt model.dungeon model.depth
  |> Maybe.withDefault Level.init

viewed : Model -> List Point
viewed model =
  let lvl = (level model) in
  lvl.viewed

exploration : Model -> (List Point, List Point)
exploration model =
  let v = viewed model in
  ((model |> floors) ++ (model |> walls))
  |> List.partition (\p -> List.member p v)

walls : Model -> List Point
walls model =
  (level model).walls

coins : Model -> List Point
coins model =
  (level model).coins

creatures : Model -> List Creature.Model
creatures model =
  (level model).creatures

doors : Model -> List Point
doors model =
  (level model).doors

floors : Model -> List Point
floors model =
  (level model).floors

upstairs : Model -> List Point
upstairs model =
  case (level model).upstairs of
    Just pt -> [pt]
    Nothing -> []

downstairs : Model -> List Point
downstairs model =
  case (level model).downstairs of
    Just pt -> [pt]
    Nothing -> []

entrances : Model -> List Point
entrances model =
  case (level model).entrance of
    Just (pt,_) -> [pt]
    Nothing -> []

crystals : Model -> List Point
crystals model =
  case (level model).crystal of
    Just (pt,_) -> [pt]
    Nothing -> []

origin = {x=0,y=0}

-- PREDICATES/QUERIES

isPlayer : Point -> Model -> Bool
isPlayer position model =
  model.player.position == position

entitiesAt : Point -> Model -> List Entity
entitiesAt pt model =
  let
    player =
      if model.player.position == pt then
        [Entity.player model.player]
      else
        []

    entities =
      Level.entitiesAt pt (level model)
  in
    entities ++ player

-- PLAYER STEP
playerSteps : Direction -> Model -> Model
playerSteps direction model =
  if not (canPlayerStep direction model) then
    model
    |> playerAttacks direction
  else
    model
    |> playerMoves direction
    |> playerAscendsOrDescends
    |> playerViewsField
    |> playerCollectsCoins
    |> playerLiberatesCrystal
    |> playerEscapesHall

playerMoves : Direction -> Model -> Model
playerMoves direction model =
  { model | player = (Warrior.step direction model.player) }

canPlayerStep : Direction -> Model -> Bool
canPlayerStep direction model =
  let
    move =
      model.player.position
      |> slide direction
  in
    not ( Level.isCreature move (level model) || List.member move (walls model))
--    not (isBlockedForPlayer move model)

--isBlockedForPlayer : Point -> Model -> Bool
--isBlockedForPlayer pt model =

playerCollectsCoins : Model -> Model
playerCollectsCoins model =
  let
    isCoin =
      level model
      |> Level.isCoin model.player.position

    dungeon' =
      model.dungeon
      |> Dungeon.collectCoin model.player.position model.depth
  in
    if (not isCoin) then
      model
    else
      let event = Event.pickupCoin in
        { model | player  = Warrior.enrich 1 model.player
                , dungeon = dungeon'
                , events  = model.events ++ [event]
        }

playerLiberatesCrystal : Model -> Model
playerLiberatesCrystal model =
  let
    isCrystal =
      level model
      |> Level.isCrystal model.player.position

    dungeon' =
      model.dungeon
      |> Dungeon.liberateCrystal model.depth
  in
    if model.crystalTaken || (not isCrystal) then
      model
    else
      let event = Event.crystalTaken in
      { model | crystalTaken = True
              , dungeon = dungeon'
              , events = model.events ++ [event]
      }

playerEscapesHall : Model -> Model
playerEscapesHall model =
  let
    isEntrance =
      level model
      |> Level.isEntrance model.player.position
  in
    if model.crystalTaken && isEntrance then
      let event = Event.hallsEscaped in
      { model | hallsEscaped = True
              , events = model.events ++ [event]
      }
    else
      model

playerAttacks : Direction -> Model -> Model
playerAttacks direction model =
  let
    {player} =
       model

    attackedPosition =
      player.position
      |> slide direction

    maybeCreature =
      level model
      |> Level.creatureAt attackedPosition
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

creatureTakesDamage : Creature.Model -> Int -> Model -> Model
creatureTakesDamage creature amount model =
  let
    dungeon' =
      model.dungeon
      |> Dungeon.injureCreature creature amount model.depth

    attackEvent =
      Event.attack creature amount
  in
    { model | dungeon = dungeon'
            , events = model.events ++ [attackEvent]
    }

removeDeceasedCreatures : Model -> Model
removeDeceasedCreatures model =
  let
    (dungeon', events') =
      Dungeon.purge model.depth model.dungeon
  in
    { model | dungeon = dungeon'
            , events = model.events ++ events' }

playerAscendsOrDescends : Model -> Model
playerAscendsOrDescends model =
  let
    playerPos =
      model.player.position
  in
    if List.member playerPos (downstairs model) && model.depth < ((List.length model.dungeon) - 1) then
      model |> playerDescends
    else
      if List.member playerPos (upstairs model) && model.depth > 0 then
        model |> playerAscends
      else
        model

playerAscends : Model -> Model
playerAscends model =
  let
    player =
      model.player

    model' =
      { model | depth = model.depth - 1 }

    player' =
      { player | position = (downstairs model') |> List.head |> Maybe.withDefault origin }

    events' =
      model.events ++ [Event.ascend (model.depth-1)]
  in
    { model' | player = player' }

playerDescends : Model -> Model
playerDescends model =
  let
    player =
      model.player

    model' =
      { model | depth = model.depth + 1 }

    player' =
      { player | position = (upstairs model') |> List.head |> Maybe.withDefault origin }

    events' =
      model.events ++ [Event.descend (model.depth+1)]
  in
    { model' | player = player', events = events' }

playerViewsField : Model -> Model
playerViewsField model =
  let
    source =
      model.player.position

    locations =
      model |> illuminate source

    --dungeon =
      --|> List.foldr (\location -> Dungeon.playerSees location model.depth) model.dungeon

  in
    --if locations == model.illuminated then
    --  model
    --else
      { model | dungeon = model.dungeon |> Dungeon.playerSees locations model.depth
              , illuminated = locations -- |> Util.uniqueBy Point.code
      }

illuminate : Point -> Model -> List Point
illuminate source model =
  let
    perimeter =
      Point.perimeter {x=1,y=1} Configuration.viewWidth Configuration.viewHeight

    blockers =
      (walls model)
      ++ (doors model)

  in
    source
    |> Optics.illuminate perimeter blockers

-- VIEW
view : Model -> List (Svg.Svg a)
view model =
  let
    litEntities =
      model.illuminated
      |> List.concatMap (\pt -> entitiesAt pt model)

    memoryEntities =
      (viewed model)
      |> List.concatMap (\pt -> entitiesAt pt model)
      |> List.filter (not << Entity.isCreature)
      |> List.map (Entity.memory)
      |> List.filter (\pt -> not (List.member pt litEntities))

    --(explored, unexplored) = 
    --  exploration model

    --unexploredEntities =
    --  unexplored
    --  |> List.concatMap (\pt -> entitiesAt pt model)
    --  |> List.map (Entity.imaginary)

    entities =
      --if model.showMap then
      --  memoryEntities ++
      --  unexploredEntities ++

      --  litEntities ++
      --  [Entity.player model.player]
      --else
        memoryEntities ++
        litEntities ++
        [Entity.player model.player]

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
    level =
      "LEVEL: " ++ toString model.depth

    gold =
      "GOLD: " ++ toString model.player.gold

    hp =
      "HP: " ++ toString model.player.hp ++ "/" ++ toString model.player.maxHp

    message =
      String.join "  |  " [ gold, hp, level ]

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
