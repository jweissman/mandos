import Direction exposing (Direction(..)) --, slide, random, describe)
import Point exposing (Point, slide)

import Warrior
import World
import Creature
import Log

import Task
import Char
import Keyboard
import Random

import Html exposing (Html)
import Html.App as App
import Html.Attributes

import Svg exposing (svg, rect, text')
import Svg.Attributes exposing (viewBox, width, x, y, fontSize, fontFamily)
import Svg.Events

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
  }

-- INIT

init : (Model, Cmd Msg)
init =
  ({world = World.init}, Cmd.none)

-- UPDATE

type Msg
  = KeyMsg Keyboard.KeyCode
  | WorldMsg World.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    WorldMsg subMsg ->
      let 
        world = World.update (subMsg) model.world
      in
        ({ model | world = world }, Cmd.none)

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
            _ -> model.world

        stepCreatureCommands =
          World.moveCreaturesCommand model.world WorldMsg
      in
        ({ model | world = world }, stepCreatureCommands)

-- SUBS
subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.presses KeyMsg

-- VIEW
view : Model -> Html Msg
view model =
  let
    worldView =
      World.view model.world

    viewBoxStyle = [
      ( "padding", "40px" )
      ]

  in
    Html.div [ Html.Attributes.style viewBoxStyle ] [
      svg [ viewBox "0 0 60 60", width "800px" ] worldView
    ]
