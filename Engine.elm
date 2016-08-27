module Engine exposing (Engine, init, view, enter, clickAt, hoverAt, tick, handleKeypress, resetHover)

import Point exposing (Point, slide)
import Direction exposing (Direction(..))
import World
import Dungeon exposing (Dungeon)
import Entity exposing (Entity)
import Configuration


import Mouse
import Util
import Time

import Graphics
import Svg exposing (svg, rect, text')
import Svg.Attributes exposing (viewBox, width, height, x, y, fontSize, fontFamily)
import Svg.Events

type alias Engine =
  { world : World.Model
  , hover : Maybe Entity
  , hoverPath : List Point
  , followPath : Maybe (List Point)
  , auto : Bool
  }

init : Engine
init =
  { world = World.init
  , hover = Nothing
  , hoverPath = []
  , followPath = Nothing
  , auto = False
  }

enter : Dungeon -> Engine -> Engine
enter dungeon model =
  let
    dungeon' =
      dungeon |> Dungeon.prepare

    world =
      model.world

    player =
      world.player

    startPos =
      case (Dungeon.levelAt 0 dungeon').entrance of
        Just (pt,_) -> pt
        Nothing -> {x=10,y=10}

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

handleKeypress : Char -> Engine -> Engine
handleKeypress keyChar model =
  model |> case keyChar of
    'k' -> playerSteps North
    'l' -> playerSteps East
    'j' -> playerSteps South
    'h' -> playerSteps West
    'x' -> playerExplores
    'a' -> autorogue
    _ -> (resetFollow << resetAuto)

tick : Time.Time -> Engine -> Engine
tick time model =
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

moveCreatures model =
  --model
  let
    world = 
      model.world

    (dungeon', events, player') =
      world.dungeon 
      |> Dungeon.moveCreatures world.player world.depth

    world' =
      { world | dungeon = dungeon'
              , events = world.events ++ events
              , player = player'
      }

  in
    --Debug.log "MOVE CREATURES!"
    { model | world = world' }

playerSteps direction model =
  let
    world' =
      (World.playerSteps direction model.world)
  in
    { model | world = world' }
            |> moveCreatures
            |> resetFollow
            |> resetAuto

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

-- maybe to utils?
screenToCoordinate : Mouse.Position -> Point
screenToCoordinate {x,y} =
  let scale = Configuration.viewScale in
  { x = x//scale
  , y = (y//scale)+1 -- ignore top bar ...
  }

hoverAt : Point -> Engine -> Engine
hoverAt position model =
  let
    point =
      (screenToCoordinate position)

    maybeEntity =
      if List.member point (model.world.illuminated) then
        --model.world.illuminated
        World.entitiesAt point model.world
        |> List.reverse
        |> List.head
      else
        if List.member point (World.viewed model.world) then
          case (World.entitiesAt point model.world |> List.reverse |> List.head) of
            Just entity ->
              Just (Entity.memory entity)
            Nothing ->
              Nothing
        else
          Nothing

    pathToEntity =
      case maybeEntity of
        Nothing ->
          []

        Just entity ->
          let
            entityPos =
              Entity.position entity

            playerPos =
              model.world.player.position

            accessible =
              (not (World.isBlocked entityPos model.world))

            alreadyHovering =
              case model.hover of
                Just entity' ->
                  entity' == entity
                Nothing -> False

          in
            if accessible then
               if alreadyHovering || not (model.followPath == Nothing) then
                 model.hoverPath
               else
                 model.world
                 |> World.path entityPos playerPos
                 |> Maybe.withDefault []
            else
              []
    in
      { model | hover = maybeEntity
              , hoverPath = pathToEntity
      }

clickAt : Point -> Engine -> Engine
clickAt point model =
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
              (Util.directionBetween nextStep model.world.player.position)

            world =
              model.world
              |> World.playerSteps direction

            onPath =
              nextStep == (playerPos |> slide direction)
          in
            if onPath then
              ({model | followPath = List.tail path
                      , world = world
              }) |> moveCreatures
            else
              model
              |> resetFollow
              |> resetHover

-- auto-assign follow path
playerExplores : Engine -> Engine
playerExplores model =
  let
    playerPos =
      model.world.player.position

    byDistanceFromPlayer =
      (\c -> Point.distance playerPos c)

    destSources =
      if model.world.crystalTaken then
        World.upstairs model.world ++ World.entrances model.world
      else
        World.downstairs model.world ++ World.crystals model.world

    maybeDest =
      destSources
      |> List.sortBy byDistanceFromPlayer
      |> List.head

    path =
      case maybeDest of
        Nothing ->
          Nothing

        Just coin ->
          model.world
          |> World.path coin playerPos

  in
    { model | followPath = path }

view : Engine -> List (Svg.Svg a) -- Html Msg
view model =
  let
    world =
      model.world

    path =
      case model.followPath of
        Nothing -> model.hoverPath
        Just path -> path

    worldView =
      World.view { world | debugPath = path }

    debugMsg =
      case model.hover of
        Nothing ->
          "You aren't looking at anything in particular."

        Just entity ->
          case entity of
            Entity.Memory e ->
              "You remember seeing " ++ (Entity.describe e) ++ " here."
            _ ->
              "You see " ++ (Entity.describe entity) ++ "."
          --(toString (Entity.position entity)) ++ " You see " ++ 
          --(Entity.describe entity) ++ "."

    note =
      Graphics.render debugMsg {x=15,y=1} "white"

    --bgStyle = [
    --  ( "background-color", "#280828"
    --  )
    --]

  in
    worldView ++ [note]
    --Html.div [ style bgStyle ] [
    --  Html.node "style" [type' "text/css"] [Html.text "@import 'https://fonts.googleapis.com/css?family=VT323'"]
      --svg [ viewBox "0 0 60 40", width "1200px", height "800px" ] (worldView ++ [note])
    --]
