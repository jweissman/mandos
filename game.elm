import Direction exposing (Direction(..))
import Point exposing (Point, slide)

import Warrior
import World
import Bfs
import Creature
import Log
import Graphics

import Entity exposing (Entity)
import Dungeon exposing (Dungeon)
import Util

import Char
import Task
import Keyboard exposing (KeyCode)
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
  , auto : Bool
  }

-- INIT
init : (Model, Cmd Msg)
init =
  (
    { world = World.init
    , hover = Nothing
    , hoverPath = []
    , followPath = Nothing
    , auto = False
    }
    , Random.generate MapMsg (Dungeon.generate 10)
  )

-- TYPES
type Msg
  = KeyMsg KeyCode
  | HoverMsg Mouse.Position
  | ClickMsg Mouse.Position
  | WorldMsg World.Msg
  | TickMsg Time
  | MapMsg Dungeon

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    MapMsg dungeon ->
      let
        world =
          model.world

        player =
          world.player

        --floors =
        --  (Dungeon.levelAt 0 dungeon).floors
          --World.floors world'

        player' =
          { player | position = (Dungeon.levelAt 0 dungeon).upstairs } --(List.head floors |> Maybe.withDefault {x=5,y=5}) }

        world' =
          { world | dungeon = dungeon
                  , depth = 0
                  , player = player'
          }
      in
      ({ model | world = world' }, Cmd.none)

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
      case model.followPath of
        Nothing ->
          if model.auto then
            (model |> playerExplores, Cmd.none)
          else
            (model, Cmd.none)

        Just path ->
          --let 
          --  cmds = World.turnCreaturesCommand model.world WorldMsg 
          --in
          (model |> playerFollowsPath, Cmd.none) -- cmds)

    KeyMsg keyCode ->
      let
        keyChar =
          (Char.fromCode keyCode)

        --turnCreatureCommands =
        --  World.turnCreaturesCommand model.world WorldMsg
      in
        (model
         |> handleKeypress keyChar
         --|> moveCreatures
         |> resetHover
        , Cmd.none) -- turnCreatureCommands)

handleKeypress : Char -> Model -> Model
handleKeypress keyChar model =
  model |> case keyChar of
    'k' -> playerSteps North
    'l' -> playerSteps East
    'j' -> playerSteps South
    'h' -> playerSteps West
    'x' -> playerExplores
    'a' -> autorogue
    _ -> (resetFollow << resetAuto)

autorogue model = 
  { model | auto = True }

playerSteps direction model =
  let 
    world = 
      (World.playerSteps direction model.world) 
  in
    { model | world = world } 
    |> resetFollow 
    |> resetAuto

resetHover : Model -> Model
resetHover model =
  { model | hoverPath = []
          , hover = Nothing }

resetFollow : Model -> Model
resetFollow model =
  { model | followPath = Nothing }

resetAuto : Model -> Model
resetAuto model =
  { model | auto = False }

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
                Just entity' ->
                  entity' == entity
                Nothing -> False

          in
            if accessible then
               if alreadyHovering || not (model.followPath == Nothing) then
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

            (dungeon', events, player') =
              world.dungeon 
              |> Dungeon.moveCreatures world.player world.depth

            world' =
              { world | dungeon = dungeon'
                      , events = world.events ++ events
                      , player = player'
              }
          in
            if (nextStep == (playerPos |> slide direction)) then
              ({model | followPath = List.tail path
                      , world = world
              })
              --|> Dungeon.moveCreatures
            else
              model
              |> resetFollow
              |> resetHover

-- auto-assign follow path
playerExplores : Model -> Model
playerExplores model =
  let
    playerPos =
      model.world.player.position

    byDistanceFromPlayer =
      (\c -> Point.distance playerPos c)

    maybeCoin =
      World.coins model.world --0.coins
      |> List.sortBy byDistanceFromPlayer
      |> List.head

    path =
      case maybeCoin of
        Nothing ->
          Nothing

        Just coin ->
          Bfs.bfs playerPos (\p -> p == coin) model.world

  in
    { model | followPath = path } -- |> playerFollowsPath

-- SUBS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.moves HoverMsg
    , Mouse.clicks ClickMsg
    , Keyboard.presses KeyMsg
    , Time.every (150*millisecond) TickMsg
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
          (toString (Entity.position entity)) ++ " You see " ++ (Entity.describe entity) ++ "."

    note =
      Graphics.render debugMsg {x=15,y=1} "white"

    bgStyle = [
      ( "background-color", "#280828"
      )
    ]

  in
    Html.body [ style bgStyle ] [
      Html.div [ style bgStyle ] [
        Html.node "style" [type' "text/css"] [Html.text "@import 'https://fonts.googleapis.com/css?family=VT323'"]
        , svg [ viewBox "0 0 60 40", width "1200px", height "800px" ] (worldView ++ [note])
      ]
    ]

screenToCoordinate : Mouse.Position -> Point
screenToCoordinate {x,y} =
  { x = x//20
  , y = (y//20)+1
  }
