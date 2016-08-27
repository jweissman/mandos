import Direction exposing (Direction(..))
import Engine exposing (Engine)
import World
import Dungeon exposing (Dungeon)
import Entity exposing (Entity)
import Graphics

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

type GameState = Splash | Generating | Playing -- | Death

type alias Model = 
  { engine : Engine
  , state : GameState
  , generationUnderway : Bool
  }

-- INIT
init : (Model, Cmd Msg)
init = ( { engine = Engine.init, state = Splash, generationUnderway = False  }, Cmd.none ) -- generate )

generate : Cmd Msg
generate =
  Random.generate MapMsg (Dungeon.generate depth)

depth : Int
depth = 50

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
      ({ model | engine = (model.engine |> Engine.enter dungeon) 
               , state = Playing
      }, Cmd.none)

    ClickMsg position ->
      ({ model | engine = (model.engine |> Engine.clickAt position) }, Cmd.none)
      --(model |> Engine.clickAt position, Cmd.none)

    HoverMsg position ->
      ({ model | engine = (model.engine |> Engine.hoverAt position) }, Cmd.none)
      --(model |> Engine.hoverAt position, Cmd.none)

    TickMsg time ->
      case model.state of
        Splash -> (model, Cmd.none)
        Playing ->
          ({ model | engine = (model.engine |> Engine.tick time) }, Cmd.none)
        Generating ->
          if model.generationUnderway then
             (model, Cmd.none)
          else
             (model, generate)
      --(model |> Engine.tick time, Cmd.none)

    KeyMsg keyCode ->
      case model.state of
        Splash -> ({model | state = Generating}, Cmd.none) -- generate)
        Generating -> (model, Cmd.none)
        Playing ->
          let 
            keyChar = 
              Char.fromCode keyCode

            engine' =
              model.engine
              |> Engine.handleKeypress keyChar
              |> Engine.resetHover

          in
            ({ model | engine = engine'}, Cmd.none)

-- SUBS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.moves HoverMsg
    , Mouse.clicks ClickMsg
    , Keyboard.presses KeyMsg
    , Time.every (80*millisecond) TickMsg
    ]

-- VIEW
view : Model -> Html Msg
view model =
  let
    bgStyle = [
      ( "background-color", "#280828"
      )
    ]
  in
    Html.div [ style bgStyle ] 
    [ Html.node "style" [type' "text/css"] [Html.text "@import 'https://fonts.googleapis.com/css?family=VT323'"]
    , svg [ viewBox "0 0 60 40", width "1200px", height "800px" ] (stateView model)
    ]

stateView model = 
  case model.state of
    Splash ->
      [Graphics.render "MANDOS" {x=25,y=10} "white"
      ,Graphics.render "Press any key to start" {x=22, y=15} "green"
      ]

    Generating ->
      [Graphics.render "MANDOS" {x=25,y=10} "white"
      ,Graphics.render "Generating world..." {x=22, y=15} "lightgreen"
      ,Graphics.render "(This may take a little while!)" {x=22, y=16} "green"
      ]

    Playing ->
      Engine.view model.engine
