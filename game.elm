import Direction exposing (Direction(..)) --, slide, random, describe)
import Point exposing (Point, slide)

import Warrior
import World
import Bfs
import Creature
import Log
import Graphics

import Entity exposing (Entity)
import Util

import Char
import Task
import Keyboard
import Mouse
import Random
import Time exposing (Time, millisecond)

import Html exposing (Html)
import Html.App as App
import Html.Attributes exposing (type', style)

import Svg exposing (svg, rect, text')
import Svg.Attributes exposing (viewBox, width, height, x, y, fontSize, fontFamily)
import Svg.Events

-- MAIN
main =
  App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL
type alias Model =
  { world : World.Model
  , hover : Maybe Entity
  , hoverPath : List Point
  , followPath : Maybe (List Point)
  }

-- INIT
init : (Model, Cmd Msg)
init =
  ({world = World.init, hover = Nothing, hoverPath = [], followPath = Nothing}, Cmd.none)

-- TYPES
type Msg
  = KeyMsg Keyboard.KeyCode
  | HoverMsg Mouse.Position
  | ClickMsg Mouse.Position
  | WorldMsg World.Msg
  | TickMsg Time.Time

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    WorldMsg subMsg ->
      let
        world = 
          model.world
          |> World.update subMsg
      in
        ({ model | world = world }, Cmd.none)

    ClickMsg position ->
      (model |> clickAt position, Cmd.none)

    HoverMsg position ->
      (model |> hoverAt position, Cmd.none)

    TickMsg time ->
      (model |> playerFollowsPath,
      --, Cmd.none)
          World.turnCreaturesCommand model.world WorldMsg)

    KeyMsg keyCode ->
      let
        keyChar =
          (Char.fromCode keyCode)

        turnCreatureCommands =
          World.turnCreaturesCommand model.world WorldMsg
      in
        (model
         |> handleKeypress keyChar
         |> moveCreatures 
         |> resetHover
        , turnCreatureCommands)

handleKeypress : Char -> Model -> Model
handleKeypress keyChar model =
  let
    updatedWorld =
      case keyChar of
        'k' -> World.playerSteps North model.world
        'l' -> World.playerSteps East  model.world
        'j' -> World.playerSteps South model.world
        'h' -> World.playerSteps West  model.world
        --'x' -> World.playerExplores model.world
        _ -> model.world

  in { model | world = updatedWorld }

moveCreatures : Model -> Model
moveCreatures model =
  { model | world = (World.moveCreatures model.world) }

hoverAt : Point -> Model -> Model
hoverAt position model =
  let
    point =
      (screenToCoordinate position)

    maybeEntity =
      World.entityAt point model.world

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
                Just entity' -> entity' == entity
                Nothing -> False
          in
            if accessible then
               if alreadyHovering then
                 model.hoverPath 
               else
                 model.world
                 |> Bfs.bfs playerPos (\pos -> (entityPos == pos))
                 |> Maybe.withDefault []
            else
              []
    in
      { model | hover = maybeEntity
              , hoverPath = pathToEntity
         }


resetHover : Model -> Model
resetHover model =
  { model | hoverPath = []
          , hover = Nothing }


clickAt : Point -> Model -> Model
clickAt point model =
  case model.followPath of
    Nothing ->
      { model | followPath = Just model.hoverPath }
    Just path ->
      model

playerFollowsPath : Model -> Model
playerFollowsPath model =
  case model.followPath of
    Nothing -> model
    Just path ->
      case (List.head path) of
        Nothing -> { model | followPath = Nothing }
        Just nextStep ->
          ({model | followPath = List.tail path
                  , world = World.playerSteps (Util.directionBetween nextStep model.world.player.position) model.world
          }) |> moveCreatures 

-- SUBS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.moves HoverMsg
    , Mouse.clicks ClickMsg
    , Keyboard.presses KeyMsg
    , Time.every (200*millisecond) TickMsg
    ]

-- VIEW
view : Model -> Html Msg
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
          "You see " ++ (Entity.describe entity) ++ "."

    note =
      Graphics.render debugMsg {x=10,y=1} "white"

    bgStyle = [
      ( "background-color", "#280828" 
      )
    ]

  in
    Html.body [ style bgStyle ] [
      Html.div [ style bgStyle ] [
        Html.node "style" [type' "text/css"] [Html.text "@import 'https://fonts.googleapis.com/css?family=VT323'"]

        , svg [ viewBox "0 0 60 45", width "1200px", height "900px" ] (worldView ++ [note])
      ]
    ]


screenToCoordinate : Mouse.Position -> Point
screenToCoordinate {x,y} =
  { x = x//20
  , y = (y//20)+1
  }
