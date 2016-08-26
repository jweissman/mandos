import Direction exposing (Direction(..))
import Engine exposing (Engine)
import World
import Dungeon exposing (Dungeon)
import Entity exposing (Entity)
import Graphics exposing (render)

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

type alias Model = Engine

-- INIT
init : (Model, Cmd Msg)
init = ( Engine.init, generate )

generate : Cmd Msg
generate =
  Random.generate MapMsg (Dungeon.generate depth)

depth : Int
depth = 10

-- TYPES
type Msg
  = KeyMsg KeyCode
  | HoverMsg Mouse.Position
  | ClickMsg Mouse.Position
  | TickMsg Time
  | MapMsg Dungeon

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    MapMsg dungeon ->
      ((model |> Engine.enter dungeon), Cmd.none)

    ClickMsg position ->
      (model |> Engine.clickAt position, Cmd.none)

    HoverMsg position ->
      (model |> Engine.hoverAt position, Cmd.none)

    TickMsg time ->
      (model |> Engine.tick time, Cmd.none)

    KeyMsg keyCode ->
      let keyChar = (Char.fromCode keyCode) in
        (model
         |> Engine.handleKeypress keyChar
         |> Engine.resetHover
        , Cmd.none)

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
          case entity of
            Entity.Memory e ->
              "You remember seeing " ++ (Entity.describe e) ++ " here."
            _ ->
              "You see " ++ (Entity.describe entity) ++ "."
          --(toString (Entity.position entity)) ++ " You see " ++ 
          --(Entity.describe entity) ++ "."

    note =
      Graphics.render debugMsg {x=15,y=1} "white"

    bgStyle = [
      ( "background-color", "#280828"
      )
    ]

  in
    Html.div [ style bgStyle ] [
      Html.node "style" [type' "text/css"] [Html.text "@import 'https://fonts.googleapis.com/css?family=VT323'"]
      , svg [ viewBox "0 0 60 40", width "1200px", height "800px" ] (worldView ++ [note])
    ]
