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
import Palette
import Inventory

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
          'd' ->
            let act' = (if action == Action.drop then Action.default else Action.drop) in
            waitForSelection act'
          'i' ->
            if action == Action.drop then
              waitForSelection Action.default
            else
              resetAction
          _ ->
            playerActs (Util.fromAlpha keyChar)

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
          'i' -> waitForSelection Action.default
          _ -> reset

waitForSelection : Action -> Engine -> Engine
waitForSelection action model =
  --let
  --  nothingToSelect =
  --    List.length model.world.player.inventory == 0
  --    && model.world.player.weapon == Nothing
  --    && model.world.player.armor == Nothing
  --in
  --  if nothingToSelect then
  --    model
  --  else
  { model | action = Just action }

isEquipped : Item -> Engine -> Bool
isEquipped item model =
  let
    player =
      model.world.player

    isArmor =
      case player.armor of
        Nothing ->
          False
        Just armor ->
          item == (Item.simple (Item.armor armor))

    isWeapon =
      case player.weapon of
        Nothing ->
          False
        Just weapon ->
          item == (Item.simple (Item.weapon weapon))
  in
    isArmor
    || isWeapon

playerActs : Int -> Engine -> Engine
playerActs idx model =
  let
    maybeItem =
      model.world.player
      |> Inventory.itemAtIndex idx
  in
    case maybeItem of
      Nothing ->
        model
        |> resetAction

      Just item ->
        case model.action of
          Nothing ->
            model

          Just act ->
            if Action.canPerform (isEquipped item model) item act then
              model |> playerActsOnItem item act
            else
              model

playerActsOnItem : Item -> Action -> Engine -> Engine
playerActsOnItem item act model =
  case act of
    Drop ->
      { model | world = model.world |> World.playerDropsItem item }

    Wear ->
      { model | world = model.world |> World.playerWears item }
              |> playerLosesItem item

    TakeOff ->
      { model | world = model.world |> World.playerTakesOffArmor }

    Wield ->
      { model | world = model.world |> World.playerWields item }
              |> playerLosesItem item

    Sheathe ->
      { model | world = model.world |> World.playerSheathesWeapon }

    Drink ->
      { model | world = model.world |> World.playerDrinks item }
              |> playerLosesItem item

    Read ->
      case item.kind of
        Scroll spell ->
          model
          |> castSpell item spell
        _ ->
          model

    Use item' act' ->
      if Item.canApply item' item then --Action.canPerform item' act' then
        model
        |> playerApplies item' item
        |> waitForSelection Action.default
      else
        model
      --|> resetAction

    Default ->
      let
        equipped =
          isEquipped item model

        action' =
          Action.defaultForItem equipped item
      in
        model
        |> playerActsOnItem item action'

    _ ->
      model

playerApplies : Item -> Item -> Engine -> Engine
playerApplies item' item model =
  case item'.kind of
    Scroll spell ->
      case spell of
        Infuse ->
          --Debug.log ("USE ITEM " ++ (Item.describe item'))
          --Debug.log ("ON ITEM " ++ (Item.describe item))
          model
          |> playerLosesItem item'
          |> playerEnchants item

        _ ->
          model
    _ ->
      model

resetAction : Engine -> Engine
resetAction model =
  { model | action = Nothing }

playerLosesItem : Item -> Engine -> Engine
playerLosesItem item model =
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
    { model | world = { world | player = player' } }

castSpell : Item -> Spell -> Engine -> Engine
castSpell item spell model =
  let world = model.world in
  case spell of
    Lux ->
      model
      |> playerLosesItem item
      |> enhancePlayerVision

    Infuse ->
      --Debug.log "CAST SPELL"
      model
      |> waitForSelection (Action.use item (Action.enchant))

enhancePlayerVision : Engine -> Engine
enhancePlayerVision model =
  { model | world = model.world
          |> World.augmentVision
          |> World.playerViewsField
  }

playerEnchants : Item -> Engine -> Engine
playerEnchants item model =
  { model | world = model.world
          |> World.enchantItem item
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

pointFromMouse : Mouse.Position -> Point
pointFromMouse {x,y} =
  let scale = Configuration.viewScale in
  ( x//scale
  , (y//scale)+1
  )

hoverAt : Mouse.Position -> Engine -> Engine
hoverAt position model =
  let
    point =
      pointFromMouse position

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
      --if List.length model.world.player.inventory < Configuration.inventoryLimit then
      if Inventory.size model.world.player < Configuration.inventoryLimit then
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
      Graphics.render debugMsg (25,1) Palette.accentLighter

    quests =
      Journal.view (65,2) model.world model.quests --world.player

    character =
      model.world.player
      |> Warrior.cardView (65, 5+(List.length model.quests)) (model.action)

    inventory =
      Inventory.view (65, 10+(List.length model.quests)) model.action model.world.player --model.world

    log =
      Log.view (2, 44) model.world.events

    status =
      Status.view (1,1) model.world

    rightBar =
      quests
      ++ character
      ++ inventory

  in
    worldView
    ++ status
    ++ [note]
    ++ rightBar
    ++ log


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
