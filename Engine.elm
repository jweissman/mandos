module Engine exposing (Engine, init, view, enter, clickAt, hoverAt, tick, handleKeypress, resetHover)

import Point exposing (Point, slide)
import Direction exposing (Direction(..))
import Path

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
  , telepathy : Bool
  }

init : Engine
init =
  { world = World.init
  , hover = Nothing
  , hoverPath = []
  , followPath = Nothing
  , auto = False
  , telepathy = False
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
  let reset = (resetFollow << resetAuto << moveCreatures) in
  model |> case keyChar of
    'k' -> reset << playerSteps North -- << reset
    'l' -> reset << playerSteps East --<< reset
    'j' -> reset << playerSteps South --<< reset
    'h' -> reset << playerSteps West --<< reset
    'x' -> playerExplores
    'a' -> autorogue
    't' -> telepath
    _ -> reset -- (resetFollow << resetAuto)

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

telepath model =
  if model.telepathy then
  { model | telepathy = False }
  else
  Debug.log "TELEPATH"
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
    --Debug.log "MOVE CREATURES!"
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
        World.entitiesAt point model.world
        |> List.reverse
        |> List.head
      else
        let
          entities' =
            model.world
            |> World.entitiesAt point
            |> List.filter (not << Entity.isCreature)
        in
        if List.member point (World.viewed model.world) then
          case (entities' |> List.reverse |> List.head) of
            Just entity ->
              Just (Entity.memory entity)
            Nothing ->
              Nothing
        else
          if model.telepathy then
             World.entitiesAt point model.world
             |> List.map (Entity.imaginary)
             |> List.head
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

            --accessible =
            --  (not (World.isBlocked entityPos model.world))

            alreadyHovering =
              case model.hover of
                Just entity' ->
                  entity' == entity
                Nothing -> False

          in
            --if accessible then
               if alreadyHovering || not (model.followPath == Nothing) then
                 model.hoverPath
               else
                 --model.world
                 Path.seek entityPos playerPos (\pt -> List.member pt (World.walls model.world))
                 --|> Maybe.withDefault []
            --else
            --  []
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
              (Util.directionBetween nextStep playerPos)

            onPath =
              nextStep == (playerPos |> slide direction)

            followPath' =
              List.tail path
          in
            if onPath then
              ({model | followPath = followPath' })
              |> playerSteps direction
              |> moveCreatures
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

    (explored,unexplored) =
      --World.exploration model.world
      let v = (World.viewed model.world) in
      ((model.world |> World.floors)) -- ++ (model |> walls))
      |> List.partition (\p -> List.member p v)

    frontier =
      explored
      |> List.filter (\pt ->
        List.any (\pt' -> Point.isAdjacent pt pt') unexplored)

    visibleCreatures =
      (World.creatures model.world)
      |> List.map .position
      |> List.filter (\pt -> List.member pt (explored))

    destSources =
      if model.world.crystalTaken then
        World.upstairs model.world ++ World.entrances model.world
      else
        if List.length visibleCreatures > 0 then
          visibleCreatures
        else
          if List.length frontier > 0 then 
            frontier 
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

        Just dest ->
          let 
            path' = 
              Path.seek dest playerPos (\pt -> 
                List.member pt (World.walls model.world))
          in
            if List.length path' == 0 then
              Nothing
            else
              Just path'
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
      World.view { world | debugPath = path
                         , showMap = model.telepathy 
                       }

    debugMsg =
      case model.hover of
        Nothing ->
          "You aren't looking at anything in particular."

        Just entity ->
          case entity of
            Entity.Memory e ->
              "You remember seeing " ++ (Entity.describe e) ++ " here."
            Entity.Imaginary e ->
              "You imagine there to be " ++ (Entity.describe e) ++ " here."
            _ ->
              "You see " ++ (Entity.describe entity) ++ "."

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
