module Engine exposing (Engine, init, view, enter, speak, clickAt, hoverAt, tick, handleKeypress, resetHover, autorogue)

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
import Optics
import Language exposing (Language)

import Level

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
  , selectPosition : Bool
  , throwPath : Maybe (List Point)
  , animatingThrow : Bool
  , thrownItem : Maybe Item
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
  , selectPosition = False
  , throwPath = Nothing
  , animatingThrow = False
  , thrownItem = Nothing
  }

isPerformingAnimation : Engine -> Bool
isPerformingAnimation model =
  model.animatingThrow

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

speak : Language -> Engine -> Engine
speak language model =
  let
    world =
      model.world

    world' =
      { world | language = language }

  in
    { model | world = world' }

illuminate : Engine -> Engine
illuminate model =
  { model | world = World.playerViewsField model.world }

handleKeypress : Char -> Engine -> Engine
handleKeypress keyChar model =
  if model |> isPerformingAnimation then
    model -- ignore it until we're done animating
  else
    let
      reset = (
        resetThrow <<
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
              let alpha = Util.fromAlpha keyChar in
              if alpha == -1 then
                resetAction
              else
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
  { model | action = Just action }

waitForPosition : Action -> Engine -> Engine
waitForPosition action model =
  { model | action = Just action
          , selectPosition = True
          }

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

    isHelm =
      case player.helm of
        Nothing ->
          False
        Just helm ->
          item == (Item.simple (Item.helm helm))

    isRing =
      case player.ring of
        Nothing ->
          False
        Just ring ->
          item == (Item.simple (Item.ring ring))
  in
    isArmor
    || isWeapon
    || isHelm
    || isRing

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
      { model | world = model.world |> World.playerTakesOff item }

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
      -- todo this could be refined?
      if Item.canApply item' item then
        model
        |> playerApplies item' item
        |> waitForSelection Action.default
      else
        model

    Default ->
      let
        equipped =
          isEquipped item model

        action' =
          Action.defaultForItem equipped item
      in
        model
        |> playerActsOnItem item action'

    Throw ->
      model
      |> resetAuto
      |> waitForPosition (Action.hurl item)

    Hurl it ->
      model

    Identify ->
      model

    Look ->
      model

    Enchant ->
      model

playerApplies : Item -> Item -> Engine -> Engine
playerApplies item' item model =
  case item'.kind of
    Scroll spell ->
      case spell of
        Infuse ->
          model
          |> playerLosesItem item'
          |> playerEnchants item

        Lux ->
          model
    _ ->
      model

resetAction : Engine -> Engine
resetAction model =
  { model | action = Nothing
          , selectPosition = False
  }

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
  let
    world' =
      model.world
      |> World.playerLearnsWord (Language.wordFor (Spell.idea spell) model.world.language)

    model' =
      { model | world = world' }
  in
  case spell of
    Lux ->
      model'
      |> playerLosesItem item
      |> enhancePlayerVision

    Infuse ->
      model'
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
  if model |> isPerformingAnimation then
    Debug.log "ANIMATE MODEL"
    model
    |> animate
  else
    model
    |> followPaths
    |> updateQuests

animate : Engine -> Engine
animate model =
  if model.animatingThrow then
    model
    |> animateThrow
  else
    Debug.log "animate called but nothing being animated...?"
    model

animateThrow : Engine -> Engine
animateThrow model =
  case model.thrownItem of
    Nothing ->
      Debug.log "no thrown item, reset throw"
      model |> resetThrow

    Just item ->
      case model.throwPath of
        Nothing ->
          Debug.log "no throw path, reset throw"
          model |> resetThrow

        Just path ->
          case path |> List.head of
            Nothing ->
              Debug.log "throw path empty, reset throw"
              model |> resetThrow

            Just pt ->
              let
                item' =
                  { item | position = pt }

                model' =
                  { model | thrownItem = Just item'
                          , throwPath = List.tail path }
              in
                if List.length path > 1 then
                  model'
                else
                  { model' | world = model.world
                                   |> World.hitCreatureAt pt item' --thrownItem
                  }

resetThrow : Engine -> Engine
resetThrow model =
  let
    model' =
      { model | throwPath = Nothing -- Just model.hoverPath
              , animatingThrow = False
              , action = Nothing --Just Action.default
              , thrownItem = Nothing
              --, auto = True
              , selectPosition = False
       }
  in
    case model.thrownItem of
      Nothing -> -- but how did we get here?
        model'

      Just item ->
        Debug.log "ADD ITEM BACK TO DUNGEON"
        model'
        |> addItem item

addItem : Item -> Engine -> Engine
addItem item model =
  let
    world =
      model.world

    dungeon =
      world.dungeon
      |> Dungeon.apply (Level.addItem item) world.depth
  in
  { model | world  = { world | dungeon = dungeon }}

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


hoverAt : Point -> Engine -> Engine
hoverAt pt model =
  let
    point =
      pt
      --pointFromMouse position

    isLit =
      Set.member point (model.world.illuminated)

    wasLit =
      Set.member point (World.viewed model.world)
  in
    if model.selectPosition then
      model |> targetEntityAt point
    else
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

targetEntityAt point model =
  let
    entity' =
      World.entityAt point model.world

    entity =
      if entity' == Just (Entity.wall point) then
        Nothing
      else
        entity'

    path' =
      case entity of
        Nothing ->
          []

        Just entity' ->
          model
          |> lineToEntity entity'
  in
    { model | hover = entity
            , hoverPath = path'
    }

seeEntityAt point model =
  let
    entity =
      World.entityAt point model.world

    path' =
      case entity of
        Nothing ->
          []

        Just entity' ->
          model |> pathToEntity entity'

  in
      { model | hover = entity
              , hoverPath = path'
      }

rememberEntityAt point model =
  let
    entity =
      World.entityAt point model.world

    maybeEntity =
      case entity of
         Just entity' ->
           if (Entity.isCreature entity') then
             Nothing
           else
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
      World.entityAt point model.world

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


lineToEntity entity model =
  let
    entityPos =
      Entity.position entity

    playerPos =
      model.world.player.position

    alreadyHovering =
      case model.hover of
        Just entity' ->
          entity' == entity

        Nothing ->
          False
  in
    if alreadyHovering || not (model.throwPath == Nothing) then
      model.hoverPath
    else
      let ray = Optics.castRay (Warrior.vision model.world.player) (World.walls model.world) playerPos entityPos in
      ray
      |> List.filter (\pt -> not (Set.member pt (World.walls model.world)))

clickAt : Mouse.Position -> Engine -> Engine
clickAt _ model =
  case model.followPath of
    Nothing ->
      if model.selectPosition then
        case model.action of
          Just action ->
            case action of
              Hurl item ->
                model
                |> throwItem item

              _ ->
                Debug.log "clicked for position, but some action was associated besides hurl?"
                model
                |> waitForSelection Action.default

          Nothing ->
            Debug.log "clicked for position, but no action was associated?"
            model
            |> waitForSelection Action.default
      else
        { model | followPath = Just model.hoverPath }

    Just path ->
      model

throwItem : Item -> Engine -> Engine
throwItem item model =
  Debug.log ("THROW ITEM: " ++ (Item.name item))
  { model | throwPath      = Just model.hoverPath
          , animatingThrow = True
          , selectPosition = False
          , thrownItem     = Just item
          , auto           = False
  }
  --|> resetAuto

playerFollowsPath : Engine -> Engine
playerFollowsPath model =
  case model.followPath of
    Nothing ->
      model

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

gatherTargets : Engine -> List Point
gatherTargets model =
  let
    viewed =
      World.viewed model.world

    explored =
      World.floors model.world
      |> Set.intersect viewed

    visibleCreatures =
      (World.creatures model.world)
      |> List.map .position
      |> List.filter (\pt -> Set.member pt explored)

    visibleCoins =
      (World.coins model.world)
      |> List.filter (\pt -> Set.member pt explored)

    visibleItems =
      if Inventory.size model.world.player < Configuration.inventoryLimit then
        (World.items model.world)
        |> List.map .position
        |> List.filter (\pt -> Set.member pt explored)
      else
        []
  in
    visibleCreatures ++ visibleItems ++ visibleCoins


exploreTargets : Engine -> List Point
exploreTargets model =
  if model.world |> World.doesPlayerHaveCrystal then
    World.upstairs model.world ++ World.entrances model.world
  else
    let frontier = World.viewFrontier model.world in
    if Set.size frontier == 0 then
      World.downstairs model.world ++ World.crystals model.world
    else
      frontier |> Set.toList

autorogueDestination : Engine -> Maybe Point
autorogueDestination model =
  let
    gather =
      gatherTargets model

    byDistanceFromPlayer = \pt ->
      Point.distance model.world.player.position pt
  in
    if List.length gather > 0 then
      gather
      |> List.sortBy byDistanceFromPlayer
      |> List.head
    else
      exploreTargets model
      |> List.sortBy byDistanceFromPlayer
      |> List.head

playerExplores : Engine -> Engine
playerExplores model =
  let
    path =
      case autorogueDestination model of
        Nothing ->
          Nothing

        Just dest ->
          let
            walls =
              World.walls model.world

            blocked = \pt ->
              Set.member pt walls

            path' =
              Path.seek dest model.world.player.position blocked
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

    lang =
      world.language

    vocab =
      world.player.vocabulary

    path =
      case model.followPath of
        Nothing ->
          if model |> isPerformingAnimation then
            []
          else
            model.hoverPath

        Just path ->
          path

    worldView =
      World.view { world | debugPath = path
                         , showMap = model.telepathy
                         , animateEntities = [ model.thrownItem ]
                                           |> List.filterMap identity
                                           |> List.map Entity.item
                         }

    debugMsg =
      case model.action of
        Just action' ->
          Action.question vocab lang action'

        Nothing ->
          model |> hoverMessage vocab lang

    note =
      Graphics.render debugMsg (25,1) Palette.accentLighter

    rightBarY =
      Configuration.viewWidth - 15

    quests =
      Journal.view (rightBarY,2) model.world model.quests

    character =
      model.world.player
      |> Warrior.cardView (rightBarY, 5+(List.length model.quests)) (model.action)

    inventory =
      Inventory.view (rightBarY, 10+(List.length model.quests)) vocab lang model.action model.world.player

    log =
      Log.view (2, (Configuration.viewHeight - 6)) vocab lang model.world.events

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

hoverMessage : Language -> Language -> Engine -> String
hoverMessage vocab lang model =
  case model.hover of
    Nothing ->
      "You aren't looking at anything in particular."

    Just entity ->
      case entity of
        Entity.Memory e ->
          "You remember seeing " ++ (Entity.describe vocab lang e) ++ " here."
        Entity.Imaginary e ->
          "You imagine there is " ++ (Entity.describe vocab lang e) ++ " here."
        _ ->
          "You see " ++ (Entity.describe vocab lang entity) ++ "."
