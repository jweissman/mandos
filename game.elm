import Rogue.Geometry exposing (Direction(..), slide, randomDirection, describeDirection)

import Rogue.Warrior
import Rogue.World
import Rogue.Creature
import Rogue.Log

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
  { player : Rogue.Warrior.Model
  , world : Rogue.World.Model
  , log : Rogue.Log.Model
  }

-- INIT

init : (Model, Cmd Msg)
init =
  let
    model =
      { player = Rogue.Warrior.init
      , world  = Rogue.World.init
      , log = Rogue.Log.init
      }
  in
    (model, Cmd.none)



-- UPDATE

type Msg
  = KeyMsg Keyboard.KeyCode
  | MoveCreature Int Direction

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    MoveCreature id direction ->
      (moveCreature model id direction, Cmd.none)

    KeyMsg keyCode ->
      let
        keyChar =
          (Char.fromCode keyCode)

        updatedModel =
          case keyChar of
            'k' -> step model North
            'l' -> step model East
            'j' -> step model South
            'h' -> step model West
            _ -> model

        stepCreatureCommands =
          Cmd.batch (List.map moveCreatureRandomly model.world.creatures)

      in
        (updatedModel
        |> purgeDeceased
        |> checkPlayerLife,
        stepCreatureCommands)

moveCreatureRandomly creature =
  Random.generate (MoveCreature creature.id) randomDirection

moveCreature : Model -> Int -> Direction -> Model
moveCreature model id direction =
  let
    world =
      model.world

    creatures =
      world.creatures

    movedCreatures =
      List.map (moveIndicatedCreature id direction model) creatures

    updatedWorld = { world | creatures = movedCreatures }

    updatedModel = { model | world = updatedWorld }
  in
    updatedModel |> attackPlayer id direction

moveIndicatedCreature : Int -> Direction -> Model -> Rogue.Creature.Model -> Rogue.Creature.Model
moveIndicatedCreature id direction model creature =
  if (creature.id == id && (canCreatureStep model creature direction)) then
    { creature | position = slide creature.position direction }
  else
    creature

step : Model -> Direction -> Model
step model direction =
  if not (canPlayerStep model direction) then
    model
    |> attackCreature direction
  else
    let
      newPosition =
        slide model.player.position direction

      steppedPlayer =
        Rogue.Warrior.step direction model.player

      steppedModel =
        { model | player = steppedPlayer }
    in
      steppedModel
      |> collectCoin

collectCoin : Model -> Model
collectCoin model =
  let
    isCollectingCoin =
      Rogue.World.isCoin world player.position

    player =
      model.player

    world =
      model.world

    coinsWithoutCollectedCoin =
      List.filter (\coin -> not (coin == player.position)) world.coins
  in
    if (not isCollectingCoin) then
       model
    else
      { model |
        player = { player | gold = player.gold + 1 }
      , world  = { world | coins = coinsWithoutCollectedCoin }
      , log = model.log ++ [Rogue.Log.pickupCoinEvent]
      }

origin = {x=0, y=0}

attackCreature : Direction -> Model -> Model
attackCreature direction model =
  let
    player =
      model.player

    world =
      model.world

    attackPosition =
      slide player.position direction

    isAttacking =
      Rogue.World.isCreature world attackPosition
  in
    if (not isAttacking) then
      model
    else
      let
        attackedCreature =
          Maybe.withDefault (Rogue.Creature.createRat -1 origin) (List.head (List.filter (\c -> c.position == attackPosition) world.creatures))

        attackDamage =
          player.attack - attackedCreature.defense

        updatedCreatures =
          List.map (\creature -> if creature.id == attackedCreature.id then { creature | hp = creature.hp - attackDamage } else creature) world.creatures

        updatedWorld = { world | creatures = updatedCreatures }
      in
      { model |
        world = updatedWorld
      , log = model.log ++ [Rogue.Log.attackEvent attackedCreature attackDamage] }


attackPlayer : Int -> Direction -> Model -> Model
attackPlayer id direction model =
  let
    attackingCreature =
      Maybe.withDefault (Rogue.Creature.createRat -1 origin) (List.head (List.filter (\c -> c.id == id) model.world.creatures))

    attackPosition =
      slide attackingCreature.position direction

    isAttacking =
      attackPosition == model.player.position
  in
    if (not isAttacking) then
      model
    else
      let
        player =
          model.player

        attackDamage =
          attackingCreature.attack - player.defense

        updatedPlayer =
          { player | hp = player.hp - attackDamage }
      in
        { model | player = updatedPlayer
                , log = model.log ++ [Rogue.Log.defendEvent attackingCreature attackDamage]
                }

canPlayerStep : Model -> Direction -> Bool
canPlayerStep model direction =
  let
    newPosition =
      slide model.player.position direction

    isWall =
      Rogue.World.isWall model.world newPosition

    isCreature =
      Rogue.World.isCreature model.world newPosition

    blocked =
      isWall || isCreature
  in
    not blocked

canCreatureStep : Model -> Rogue.Creature.Model -> Direction -> Bool
canCreatureStep model creature direction =
  let
    newPosition =
      slide creature.position direction

    isWall =
      Rogue.World.isWall model.world newPosition

    isCreature =
      Rogue.World.isCreature model.world newPosition

    isPlayer =
      model.player.position == newPosition

    blocked =
      isWall || isPlayer || isCreature

  in
    (not blocked)


purgeDeceased : Model -> Model
purgeDeceased model =
  let
    world =
      model.world

    creatures =
      world.creatures

    survivors =
      List.filter (\creature -> creature.hp > 0) creatures

    killed =
      List.filter (\creature -> not (creature.hp > 0)) creatures
  in
    { model |
      world = { world | creatures = survivors }
    , log = model.log ++ (List.map Rogue.Log.killEnemyEvent killed)
    }
    
checkPlayerLife : Model -> Model
checkPlayerLife model =
  if model.player.hp <= 0 then
    -- just reset model for now?
    let
      (resetModel,_) = init
    in
      resetModel
  else
    model
  

-- SUBS
subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.presses KeyMsg

-- VIEW
view : Model -> Html Msg
view model =
  let
    playerView =
      Rogue.Warrior.view model.player

    worldView =
      Rogue.World.view model.world

    topMessage = 
      "GOLD: " ++ toString model.player.gold  ++ " | HP: " ++ toString model.player.hp ++ "/" ++ toString model.player.maxHp

    topBar =
      text' [ x "0", y "1", fontSize "1", fontFamily "Courier" ] [ Html.text topMessage ]

    viewBoxStyle = [
      ( "padding", "40px" )
      ]

    log =
      Rogue.Log.view model.log

    composedView = worldView ++ [topBar, playerView] ++ log
  in
    Html.div [ Html.Attributes.style viewBoxStyle ] [
      svg [ viewBox "0 0 60 60", width "800px" ] composedView
    ]
