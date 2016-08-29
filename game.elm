import Direction exposing (Direction(..))
import Engine exposing (Engine)
import World
import Dungeon exposing (Dungeon)
import Entity exposing (Entity)
import Graphics

import Configuration
import Event

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

type GameState = Splash | Generating | Playing | Death | Victory

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
             (model, generate)

        _ -> (model, Cmd.none)

    KeyMsg keyCode ->
      case model.state of
        Splash -> 
          ({model | state = Generating}, Cmd.none)

        Death -> 
          ({model | state = Splash, engine = Engine.init}, Cmd.none)

        Victory -> 
          ({model | state = Splash, engine = Engine.init}, Cmd.none)

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

    died =
      model.engine.world.player.hp < 1

    state' =
      if won then 
        Victory 
      else
        if died then
          Death
        else
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
      ( "background-color", "#280828"
      )
    ]
  in
    Html.div [ style bgStyle ] 
    [ Html.node "style" [type' "text/css"] [Html.text "@import 'https://fonts.googleapis.com/css?family=VT323'"]
    , box (stateView model) --svg [ box ] (stateView model)
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
      --"0 0 " ++ (toString width) ++ " "
  in
    svg [ viewBox dims, width ((toString (width'*scale)) ++ "px"), height ((toString (height'*scale)) ++ "px") ] viewModel

stateView model = 
  let 
    hero = 
      Graphics.hero "MANDOS" (27,10)

    jumbo = 
      Graphics.jumbo "@" (30,30)

    anyKey =
      Graphics.render "press any key to play" (33, 20) "lightgreen"

    trademark =
      Graphics.render "Written by Joseph Weissman // A Deep Cerulean Experience" (26, 34) "darkgray"

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
      ,Graphics.render "Generating world, please wait..." (32, 15) "lightgreen"
      ,Graphics.render "(This may take a little while!)" (32, 20) "white"
      ]

    Victory ->
      Engine.view model.engine
      ++ [
        Graphics.hero "YOU WON!" (26, 15) -- "lightgreen"
      , Graphics.render "Congratulations!" (34, 20) "white"
      , Graphics.render "You escaped the Halls of Mandos!" (31, 22) "white"
        , Graphics.render ((toString steps) ++ " steps taken") (34, 25) "white"
        , Graphics.render ((toString kills) ++ " kills") (34, 26) "white"
      ]

    Death ->
        Engine.view model.engine ++ 
        [ Graphics.hero "YOU DIED!" (23, 15) -- "lightgreen"
        , Graphics.render ((toString steps) ++ " steps taken") (34, 25) "white"
        , Graphics.render ((toString kills) ++ " kills") (34, 26) "white"
        ]

    Playing ->
      Engine.view model.engine
