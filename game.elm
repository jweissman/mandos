import Direction exposing (Direction(..)) --, slide, random, describe)
import Point exposing (Point, slide)

import Warrior
import World
import Creature
import Log
import Graphics

import Entity exposing (Entity)

import Task
import Char
import Keyboard
import Mouse
import Random

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
  }

-- INIT
init : (Model, Cmd Msg)
init =
  ({world = World.init, hover = Nothing, hoverPath = []}, Cmd.none)

-- TYPES
type Msg
  = KeyMsg Keyboard.KeyCode
  | HoverMsg Mouse.Position
  --| ClickMsg Mouse.Position
  | WorldMsg World.Msg

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    WorldMsg subMsg ->
      let
        world = World.update (subMsg) model.world
      in
        ({ model | world = world }, Cmd.none)

    --ClickMsg position ->
    --  (model, Cmd.none)

    HoverMsg position ->
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
                     |> World.bfs playerPos (\pos -> (entityPos == pos))
                     |> Maybe.withDefault []
                else
                  []
      in
        ({ model | hover = maybeEntity
                 , hoverPath = pathToEntity
         }, Cmd.none)

    KeyMsg keyCode ->
      let
        keyChar =
          (Char.fromCode keyCode)

        world =
          case keyChar of
            'k' -> World.playerSteps North model.world
            'l' -> World.playerSteps East  model.world
            'j' -> World.playerSteps South model.world
            'h' -> World.playerSteps West  model.world
            --'x' -> World.playerExplores model.world
            _ -> model.world

        turnCreatureCommands =
          World.turnCreaturesCommand model.world WorldMsg
      in
        ({model | world = world} 
         |> moveCreatures 
         |> resetHover
        , turnCreatureCommands)

moveCreatures : Model -> Model
moveCreatures model =
  { model | world = (World.moveCreatures model.world) }

resetHover : Model -> Model
resetHover model =
  { model | hoverPath = []
          , hover = Nothing }

screenToCoordinate : Mouse.Position -> Point
screenToCoordinate {x,y} =
  { x = x//20
  , y = (y//20)+1
  }


-- SUBS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.moves HoverMsg
    --, Mouse.clicks ClickMsg
    , Keyboard.presses KeyMsg
    ]

-- VIEW
view : Model -> Html Msg
view model =
  let
    world =
      model.world

    worldView =
      World.view { world | debugPath = model.hoverPath }

    debugMsg =
      case model.hover of
        Nothing ->
          "You aren't looking at anything in particular."

        Just entity ->
          "You see " ++ (Entity.describe entity) ++ "."

    note =
      Graphics.render debugMsg {x=10,y=1} "white"

    viewBoxStyle = [
      ( "background-color", "#280828" )
      ]

  in
    Html.div [ style viewBoxStyle ] [
      Html.node "style" [type' "text/css"] [Html.text "@import 'https://fonts.googleapis.com/css?family=Source+Code+Pro:300|VT323'"]
      , svg [ viewBox "0 0 60 45", width "1200px", height "900px" ] (worldView ++ [note])
    ]
