module Engine exposing (Engine, init, view, enter, clickAt, hoverAt, tick, handleKeypress, resetHover)

import Point exposing (Point, slide)
import Direction exposing (Direction(..))
import Path
import World
import Dungeon exposing (Dungeon)
import Entity exposing (Entity)
import Configuration
import Util
import Graphics
import Warrior
import Quest exposing (Quest)
import Journal
import Log
import Status
import Item exposing (Item, ItemKind(..))
import Spell exposing (Spell(..))
import Action exposing (Action(..))

import Set exposing (Set)
import Time
import Mouse
import Svg exposing (svg, rect, text')
import Svg.Attributes exposing (viewBox, width, height, x, y, fontSize, fontFamily)
import Svg.Events

type alias Engine =
  { world : World.Model
  , hover : Maybe Entity
  , hoverPath : List Point
  , followPath : Maybe (List Point)
  , auto : Bool
  , telepathy : Bool
  , quests : List Quest
  , action : Maybe Action
  }


init : Engine
init =
  { world = World.init
  , hover = Nothing
  , hoverPath = []
  , followPath = Nothing
  , auto = False
  , telepathy = False
  , quests = Quest.coreCampaign
  , action = Nothing
  }

enter : Dungeon -> Engine -> Engine
enter dungeon model =
  let
    dungeon' =
      dungeon |> Dungeon.prepare Configuration.levelCount

    world =
      model.world

    player =
      world.player

    startPos =
      case (Dungeon.levelAt 0 dungeon').entrance of
        Just (pt,_) -> pt
        Nothing -> (10,10)

    player' =
      { player | position = startPos }

    world' =
      { world | dungeon = dungeon'
              , depth = 0
              , player = player'
      }
  in
  ({ model | world = world' |> World.playerViewsField
   })

illuminate : Engine -> Engine
illuminate model =
  { model | world = World.playerViewsField model.world }

handleKeypress : Char -> Engine -> Engine
handleKeypress keyChar model =
  let
    reset = (
      resetAction <<
      resetFollow <<
      resetAuto <<
      illuminate <<
      moveCreatures
    )
  in
    case model.action of
      Just action ->
        model |> case keyChar of
          '0' -> playerActs 0
          '1' -> playerActs 1
          '2' -> playerActs 2
          '3' -> playerActs 3
          '4' -> playerActs 4
          '5' -> playerActs 5
          '6' -> playerActs 6
          '7' -> playerActs 7
          '8' -> playerActs 8
          '9' -> playerActs 9
          _ -> resetAction

      Nothing ->
        model |> case keyChar of
          'a' -> autorogue
          'h' -> reset << playerSteps West
          'j' -> reset << playerSteps South
          'k' -> reset << playerSteps North
          'l' -> reset << playerSteps East
          't' -> telepath
          'x' -> playerExplores
          'd' -> waitForSelection Action.drop
          '0' -> playerUses 0
          '1' -> playerUses 1
          '2' -> playerUses 2
          '3' -> playerUses 3
          '4' -> playerUses 4
          '5' -> playerUses 5
          '6' -> playerUses 6
          '7' -> playerUses 7
          '8' -> playerUses 8
          '9' -> playerUses 9
          _ -> reset

waitForSelection : Action -> Engine -> Engine
waitForSelection action model =
  if List.length model.world.player.inventory > 0 then
    { model | action = Just action }
  else
    model

playerActs : Int -> Engine -> Engine
playerActs idx model =
  let
    maybeItem =
      Util.getAt model.world.player.inventory idx
  in
    case maybeItem of
      Nothing ->
        model

      Just item ->
        case model.action of
          Nothing ->
            model

          Just act ->
            if Action.canPerform item act then
              case act of
                Drop ->
                  model |> playerDrops idx

                _ ->
                  model

           else
             model

playerDrops : Int -> Engine -> Engine
playerDrops idx model =
  { model | world = model.world |> World.playerDropsItem idx }

resetAction : Engine -> Engine
resetAction model =
  { model | action = Nothing }

playerUses : Int -> Engine -> Engine
playerUses itemIdx model =
  let
    maybeItem =
      Util.getAt model.world.player.inventory itemIdx

  in case maybeItem of
    Nothing ->
      model

    Just item ->
      model
      |> playerUsesItem item
      --{ model | world = model.world |> World.playerUsesItem item }

playerUsesItem : Item -> Engine -> Engine
playerUsesItem item model =
  let
    world =
      model.world

    player =
      world.player

    inventory =
      player.inventory

    inventory' =
      inventory
      |> List.filter (\it -> not (it == item))

    player' =
      { player | inventory = inventory' }

  in
    case item.kind of
      Item.Arm weapon' ->
        { model | world =
          { world | player = player' |> Warrior.wield weapon' }
        }

      Item.Shield armor' ->
        { model | world =
          { world | player = player' |> Warrior.wear armor' }
        }

      Item.Bottle liquid' ->
        { model | world =
          { world | player = player' |> Warrior.drink liquid' }
        }

      -- maybe describe them?
      Item.QuestItem _ ->
         model

      Item.Scroll spell' ->
        { model | world = { world | player = player' } } --|> Warrior.cast spell' }
        --model
        |> castSpell spell'
        --|> playerViewsField

castSpell : Spell -> Engine -> Engine
castSpell spell model =
  let world = model.world in
  case spell of
    Lux ->
      model |> enhancePlayerVision
      --{ model | world = 
      --  { world | player = model.player |> Warrior.augmentVision 1 }
      --          |> World.playerViewsField
      --}

    _ -> model

enhancePlayerVision : Engine -> Engine
enhancePlayerVision model =
  --Debug.log "ENHANCE VISION"
  { model | world = model.world 
          |> World.augmentVision 
          |> World.playerViewsField
  }

tick : Time.Time -> Engine -> Engine
tick time model =
  model
  |> followPaths
  |> updateQuests


updateQuests : Engine -> Engine
updateQuests model =
  let
    quests' =
      model.quests
      |> Quest.unlocked model.world
  in
    { model | quests = quests' ++ model.quests }

followPaths : Engine -> Engine
followPaths model =
  case model.followPath of
    Nothing ->
      if model.auto then
        model
        |> playerExplores
      else
        model

    Just path ->
      model
      |> playerFollowsPath

autorogue model =
  { model | auto = True }

telepath model =
  if model.telepathy then
    { model | telepathy = False }
  else
    { model | telepathy = True }

moveCreatures model =
  let
    world =
      model.world

    (dungeon', events, player') =
      world.dungeon
      |> Dungeon.moveCreatures world.player world.depth

    world' =
      { world | dungeon = dungeon'
              , events = world.events ++ List.reverse events
              , player = player'
      }

  in
    { model | world = world' }

playerSteps direction model =
  { model | world = model.world |> World.playerSteps direction }

resetHover : Engine -> Engine
resetHover model =
  { model | hoverPath = []
          , hover = Nothing }

resetFollow : Engine -> Engine
resetFollow model =
  { model | followPath = Nothing }

resetAuto : Engine -> Engine
resetAuto model =
  { model | auto = False }

hoverAt : Mouse.Position -> Engine -> Engine
hoverAt position model =
  let
    point =
      Point.fromMouse position

    isLit =
      List.member point (model.world.illuminated)

    wasLit =
      List.member point (World.viewed model.world)
  in
    if isLit then
      model |> seeEntityAt point
    else
      if wasLit then
        model |> rememberEntityAt point
      else
        if model.telepathy then
          model |> imagineEntityAt point
        else
          model

seeEntityAt point model =
  let
    entities =
      World.entitiesAt point model.world

    maybeEntity =
      entities
      |> List.reverse
      |> List.head

    path' =
      case maybeEntity of
        Nothing ->
          []

        Just entity ->
          model |> pathToEntity entity

  in
      { model | hover = maybeEntity
              , hoverPath = path'
      }

rememberEntityAt point model =
  let
    entity =
      World.entitiesAt point model.world
      |> List.filter (not << Entity.isCreature)
      |> List.reverse
      |> List.head

    maybeEntity =
      case entity of
         Just entity' ->
           Just (Entity.memory entity')
         Nothing ->
           Nothing

    path' =
      case maybeEntity of
        Nothing ->
          []

        Just entity ->
          model |> pathToEntity entity
    in
      { model | hover = maybeEntity
              , hoverPath = path'
      }


imagineEntityAt point model =
  let
    entity =
      World.entitiesAt point model.world
      |> List.reverse
      |> List.head

    maybeEntity =
      case entity of
        Just entity' ->
          Just (Entity.imaginary entity')
        Nothing ->
          Nothing

    path' =
      case maybeEntity of
        Nothing ->
          []

        Just entity ->
          model |> pathToEntity entity
  in
    { model | hover = maybeEntity
            , hoverPath = path'
    }

pathToEntity entity model =
  let
    entityPos =
      Entity.position entity

    playerPos =
      model.world.player.position

    alreadyHovering =
      case model.hover of
        Just entity' ->
          entity' == entity
        Nothing -> False

  in
    if alreadyHovering || not (model.followPath == Nothing) then
      model.hoverPath
    else
      Path.seek entityPos playerPos (\pt -> Set.member pt (World.walls model.world))


clickAt : Mouse.Position -> Engine -> Engine
clickAt _ model =
  case model.followPath of
    Nothing ->
      { model | followPath = Just model.hoverPath }
    Just path ->
      model

playerFollowsPath : Engine -> Engine
playerFollowsPath model =
  case model.followPath of
    Nothing -> model
    Just path ->
      case (List.head path) of
        Nothing ->
          model
          |> resetHover
          |> resetFollow

        Just nextStep ->
          let
            playerPos =
              model.world.player.position

            direction =
              (Point.towards nextStep playerPos)

            onPath =
              nextStep == (playerPos |> slide direction)

            followPath' =
              List.tail path
          in
            if onPath then
              ({model | followPath = followPath' })
              |> playerSteps direction
              |> moveCreatures
              |> illuminate
            else
              model
              |> resetFollow
              |> resetHover

playerExplores : Engine -> Engine
playerExplores model =
  let
    playerPos =
      model.world.player.position

    walls' =
      World.walls model.world

    byDistanceFromPlayer =
      (\pt -> Point.distance playerPos pt)

    viewed =
      World.viewed model.world

    (explored,unexploredFloorsAndDoors) =
      (Set.union (World.floors model.world) (World.doors model.world))
      |> Set.partition (\p -> List.member p viewed)

    unexplored =
      (unexploredFloorsAndDoors)
      |> Set.union (World.walls model.world |> Set.filter (\pt -> not (List.member pt viewed)))

    frontier =
      explored
      |> Set.filter (\pt -> List.any (Point.isAdjacent pt) (unexplored |> Set.toList))

    visibleCreatures =
      (World.creatures model.world)
      |> List.map .position
      |> List.filter (\pt -> Set.member pt (explored))

    visibleCoins =
      (World.coins model.world)
      |> List.filter (\pt -> Set.member pt (explored))

    visibleItems =
      if List.length model.world.player.inventory < Configuration.inventoryLimit then
        (World.items model.world)
        |> List.map .position
        |> List.filter (\pt -> Set.member pt (explored))
      else
        []

    visibleThings =
      visibleCreatures ++ visibleItems ++ visibleCoins

    gatherAndExplore =
      if model.world |> World.doesPlayerHaveCrystal then
        visibleCoins
      else if (List.length visibleThings > 0) then
        visibleThings
      else
        frontier |> Set.toList

    ascendOrDescend =
      if model.world |> World.doesPlayerHaveCrystal then
        World.upstairs model.world ++ World.entrances model.world
      else
        if Set.size frontier == 0 then
          World.downstairs model.world ++ World.crystals model.world
        else
          []

    destSources =
      gatherAndExplore ++ ascendOrDescend

    maybeDest =
      destSources
      |> List.sortBy byDistanceFromPlayer
      |> List.head

    path =
      case maybeDest of
        Nothing ->
          Nothing

        Just dest ->
          let
            path' =
              Path.seek dest playerPos (\pt ->
                Set.member pt (World.walls model.world))
          in
            if List.length path' == 0 then
              Nothing
            else
              Just path'
  in
    { model | followPath = path }

-- VIEW
view : Engine -> List (Svg.Svg a)
view model =
  let
    world =
      model.world

    path =
      case model.followPath of
        Nothing ->
          model.hoverPath

        Just path ->
          path

    worldView =
      World.view { world | debugPath = path
                         , showMap = model.telepathy
                         }

    debugMsg =
      case model.action of
        Just action' ->
          Action.question action'

        Nothing ->
          model |> hoverMessage

    note =
      Graphics.render debugMsg (25,1) "rgba(160,160,160,0.6)"

    quests =
      Journal.view (55,2) model.world model.quests

    character =
      model.world.player
      |> Warrior.cardView (55, 5 + (List.length model.quests)) (model.action)

    log =
      Log.view (2, 37) model.world.events

    status =
      Status.view (0,1) model.world

    rightBar =
      quests
      ++ character
      ++ log
  in
    worldView
    ++ [status, note]
    ++ rightBar


hoverMessage : Engine -> String
hoverMessage model =
  case model.hover of
    Nothing ->
      "You aren't looking at anything in particular."

    Just entity ->
      case entity of
        Entity.Memory e ->
          "You remember seeing " ++ (Entity.describe e) ++ " here."
        Entity.Imaginary e ->
          "You imagine there is " ++ (Entity.describe e) ++ " here."
        _ ->
          "You see " ++ (Entity.describe entity) ++ "."
