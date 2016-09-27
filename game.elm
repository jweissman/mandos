import Direction exposing (Direction(..))
import Engine exposing (Engine)
import World
import Dungeon exposing (Dungeon)
import Entity exposing (Entity)
import Graphics
import Configuration
import Event exposing (Event(..))
import Palette
import Language exposing (Language)
import Point exposing (Point)

import Char
import Task
import Random
import String

import Time exposing (Time, millisecond)
import Keyboard exposing (KeyCode)
import Mouse
import Window

import Html exposing (Html)
import Html.App as App
import Html.Attributes exposing (type', style)

import Svg exposing (svg, rect, text')
import Svg.Attributes exposing (viewBox, width, height, x, y, fontSize, fontFamily, preserveAspectRatio)
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
  , ticks : Int
  , autoplay : Bool
  , width : Int
  , height : Int
  }

-- INIT
init : (Model, Cmd Msg)
init = ( { engine = Engine.init
         , state = Splash
         , generationUnderway = False
         , ticks = 0
         , autoplay = False
         , width = 0
         , height = 0
         },
         --Cmd.none
         Task.perform (\_ -> NoOp) sizeToMsg Window.size
       )

generateMap : Cmd Msg
generateMap =
  Random.generate MapMsg (Dungeon.generate Configuration.levelCount)

generateLanguage : Cmd Msg
generateLanguage =
  Random.generate LangMsg (Language.generate)

-- TYPES
type Msg
  = KeyMsg KeyCode
  | HoverMsg Mouse.Position
  | ClickMsg Mouse.Position
  | TickMsg Time
  | MapMsg Dungeon
  | LangMsg Language
  | ResizeWindow (Int, Int)
  | NoOp

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    NoOp ->
      (model, Cmd.none)

    ResizeWindow (width, height) ->
      ({ model | width = width, height = height }, Cmd.none)

    MapMsg dungeon ->
      ({ model | engine = (model.engine |> Engine.enter dungeon)
      }, generateLanguage)

    LangMsg language ->
      ({ model | engine = model.engine |> Engine.speak language
               , state = Playing
      }, Cmd.none)

    ClickMsg position ->
      ({ model | engine = (model.engine |> Engine.clickAt position) }, Cmd.none)

    HoverMsg position ->
      ({ model | engine = (model.engine |> Engine.hoverAt (pointFromMouse position model)) }, Cmd.none)

    TickMsg time ->
      case model.state of
        Playing ->
          let
            model' = if model.autoplay then
              { model | engine = model.engine |> Engine.autorogue
                      , ticks = 0
                      , autoplay = False }
            else
              model
          in
            ({ model' | engine = (model'.engine |> Engine.tick time) }
             |> inferState
            , Cmd.none)

        Generating ->
          if model.generationUnderway then
             (model, Cmd.none)
          else
             ({ model | generationUnderway = True }, generateMap)

        Splash ->
          if model.ticks > 200 then
            ({model | autoplay = True} |> startGeneration, Cmd.none)
          else
            ({ model | ticks = model.ticks + 1}, Cmd.none)

        _ -> (model, Cmd.none)

    KeyMsg keyCode ->
      case model.state of
        Splash ->
          (model |> startGeneration, Cmd.none)

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


pointFromMouse : Mouse.Position -> Model -> Point
pointFromMouse {x,y} model =
  let
    yScale =
      (Configuration.viewHeight / (toFloat (model.height))) -- / Configuration.viewWidth)
      |> Debug.log "scale"

    xScale =
      (Configuration.viewWidth / (toFloat (model.width))) -- / Configuration.viewWidth)
      |> Debug.log "scale"
  in
    ( round ((toFloat x)*yScale) , round ((toFloat y)*yScale))
    |> Debug.log "point"


startGeneration : Model -> Model
startGeneration model =
  {model | state = Generating
         , generationUnderway = False
         , engine = Engine.init
       }


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
    , Window.resizes sizeToMsg
    ]

sizeToMsg : Window.Size -> Msg
sizeToMsg size =
  ResizeWindow (size.width, size.height)


-- VIEW
view : Model -> Html Msg
view model =
  let
    bgStyle = [
      ( "background-color", "black"
      )
    ]
  in
    Html.div [ style bgStyle ]
    [ Html.node "style" [type' "text/css"] [Html.text "@import 'https://fonts.googleapis.com/css?family=VT323'"]
    , box (stateView model) model
    ]

box viewModel model =
  let
    dims =
      [0,0,Configuration.viewWidth,Configuration.viewHeight]
      |> List.map toString
      |> String.join " "
  in
    svg [ viewBox dims
        , style [("height", (toString model.height))
                ,("width", (toString model.width))]
        , preserveAspectRatio "xMinYMin" ] 
        viewModel

stateView model =
  let
    hero =
      Graphics.hero "MANDOS" 20

    jumbo =
      Graphics.jumbo "@"

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
            Graphics.hero "YOU WON!" 20
          , Graphics.render "Congratulations!" (34, 30) Palette.secondaryLighter
          , Graphics.render "You escaped the Halls of Mandos!" (31, 32) Palette.secondaryLight
          , Graphics.render ((toString steps) ++ " steps taken") (38, 36) Palette.secondaryLight
          , Graphics.render ((toString kills) ++ " kills") (38, 37) Palette.secondaryLight
          ]

      Death cause ->
          Engine.view model.engine ++
          [ Graphics.hero "YOU DIED!" 20
          , Graphics.render ("You fought bravely, but were " ++ cause) (35, 30) Palette.bright
          , Graphics.render ((toString steps) ++ " steps taken") (38, 36) Palette.secondaryLight
          , Graphics.render ((toString kills) ++ " kills") (38, 37) Palette.secondaryLight
          ]

      Playing ->
        Engine.view model.engine
