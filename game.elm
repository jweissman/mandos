import Direction exposing (Direction(..))
import Engine exposing (Engine)
import World
import Dungeon exposing (Dungeon)
import Entity exposing (Entity)
import Graphics
import Configuration
import Event exposing (Event(..))
import Palette

import Char
import Task
import Keyboard exposing (KeyCode)
import Mouse
import Random
import Time exposing (Time, millisecond)
import String

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

type GameState = Splash
               | Generating
               | Playing
               | Death String
               | Victory

type alias Model =
  { engine : Engine
  , state : GameState
  , generationUnderway : Bool
  }

-- INIT
init : (Model, Cmd Msg)
init = ( { engine = Engine.init
         , state = Splash
         , generationUnderway = False
         },
         Cmd.none
       )

generate : Cmd Msg
generate =
  Random.generate MapMsg (Dungeon.generate Configuration.levelCount)

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

    HoverMsg position ->
      ({ model | engine = (model.engine |> Engine.hoverAt position) }, Cmd.none)

    TickMsg time ->
      case model.state of
        Playing ->
          ({ model | engine = (model.engine |> Engine.tick time) }
           |> inferState
         , Cmd.none)

        Generating ->
          if model.generationUnderway then
             (model, Cmd.none)
          else
             ({ model | generationUnderway = True }, generate)

        _ -> (model, Cmd.none)

    KeyMsg keyCode ->
      case model.state of
        Splash ->
          ({model | state = Generating
                  , generationUnderway = False
                  , engine = Engine.init
                  },
                  Cmd.none)

        Death _ ->
          ({model | state = Splash}, Cmd.none)

        Victory ->
          ({model | state = Splash}, Cmd.none)

        Generating ->
          (model, Cmd.none)

        Playing ->
          let
            keyChar =
              Char.fromCode keyCode

            engine' =
              model.engine
              |> Engine.handleKeypress keyChar
              |> Engine.resetHover
          in
            ({ model | engine = engine' } |> inferState, Cmd.none)

inferState : Model -> Model
inferState model =
  let
    won =
      model.engine.world.hallsEscaped

    deathEvent =
      model.engine.world
      |> World.deathEvent

    state' =
      if won then
        Victory
      else
        case deathEvent of
          Just event ->
            case event of
              Event.Death cause ->
                Death cause
              _ ->
                Death "unknown causes"

          Nothing ->
            Playing

  in
   { model | state = state' }

-- SUBS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.moves HoverMsg
    , Mouse.clicks ClickMsg
    , Keyboard.presses KeyMsg
    , Time.every Configuration.tickInterval TickMsg
    ]

-- VIEW
view : Model -> Html Msg
view model =
  let
    bgStyle = [
      ( "background-color", "black" -- Palette.tertiaryDarker
      )
    ]
  in
    Html.div [ style bgStyle ]
    [ Html.node "style" [type' "text/css"] [Html.text "@import 'https://fonts.googleapis.com/css?family=VT323'"]
    , box (stateView model)
    ]

box viewModel =
  let
    scale =
      Configuration.viewScale

    height' =
      Configuration.viewHeight

    width' =
      Configuration.viewWidth

    dims =
      [0,0,width',height']
      |> List.map toString
      |> String.join " "
  in
    svg [ viewBox dims, width ((toString (width'*scale)) ++ "px"), height ((toString (height'*scale)) ++ "px") ] viewModel


stateView model =
  let
    heroPos =
      (25, 20)
 
    hero =
      Graphics.hero "MANDOS" heroPos

    jumbo =
      Graphics.jumbo "@" (32,42)

    anyKey =
      Graphics.render "press any key to play" (42, 36) Palette.bright

    trademark =
      Graphics.render "Written by Joseph Weissman // A Deep Cerulean Experience" (34, 38) Palette.tertiaryLighter

    steps =
      model.engine.world.player.steps

    kills =
      model.engine.world.events
      |> List.filter Event.isEnemyKill
      |> List.length
  in
    case model.state of
      Splash ->
        [ jumbo
        , hero
        , anyKey
        , trademark
        ]

      Generating ->
        [ jumbo
        , hero
        , Graphics.render "Generating world, please wait..." (38, 35) Palette.secondaryLighter
        , Graphics.render "(This may take a little while!)" (38, 38) Palette.secondaryLight
        ]

      Victory ->
        Engine.view model.engine
        ++ [
            Graphics.hero "YOU WON!" (20,20)
          , Graphics.render "Congratulations!" (34, 30) Palette.secondaryLighter
          , Graphics.render "You escaped the Halls of Mandos!" (31, 32) Palette.secondaryLight
          , Graphics.render ((toString steps) ++ " steps taken") (38, 36) Palette.secondaryLight
          , Graphics.render ((toString kills) ++ " kills") (38, 37) Palette.secondaryLight
          ]

      Death cause ->
          Engine.view model.engine ++
          [ Graphics.hero "YOU DIED!" (20,20)
          , Graphics.render ("You fought bravely, but were " ++ cause) (35, 30) Palette.bright
          , Graphics.render ((toString steps) ++ " steps taken") (38, 36) Palette.secondaryLight
          , Graphics.render ((toString kills) ++ " kills") (38, 37) Palette.secondaryLight
          ]

      Playing ->
        Engine.view model.engine
