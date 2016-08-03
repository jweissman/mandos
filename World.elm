module Rogue.World exposing (Model, init, view, isWall, isCoin, isCreature)

import Rogue.Geometry exposing (Point)
import Rogue.Creature exposing (Model, view, createRat)

import Html
import Svg exposing (text')
import Svg.Attributes exposing (x,y,fontSize,fontFamily)

-- MODEL

type alias Model =
  { walls : List Point
  , coins : List Point
  , creatures : List Rogue.Creature.Model
  }

-- INIT
init : Model
init =
  { walls = assembleWalls
  , coins = [{x=4,y=3}, {x=9,y=8}, {x=3,y=7}, {x=8,y=3}]
  , creatures = 
      [ Rogue.Creature.createRat 1 {x=3,y=3} 
      , Rogue.Creature.createMonkey 2 {x=6,y=9}
      ]
  }

-- just a little four walled room for now...
assembleWalls =
  List.map (\x -> {x=x,y=2}) [0..10] ++
  List.map (\x -> {x=x,y=10}) [0..10] ++
  List.map (\y -> {x=0,y=y}) [2..10] ++
  List.map (\y -> {x=10,y=y}) [2..10]

-- HELPER

isWall : Model -> Point -> Bool
isWall model position =
  List.any (\pos -> pos == position) model.walls

isCoin : Model -> Point -> Bool
isCoin model position =
  List.any (\pos -> pos == position) model.coins

isCreature : Model -> Point -> Bool
isCreature model position =
  List.any (\pos -> pos == position) (List.map .position model.creatures)

-- VIEW
view : Model -> List (Svg.Svg a)
view model =
  let
    walls =
      List.map wallView model.walls

    coins =
      List.map coinView model.coins

    creatures =
      List.map Rogue.Creature.view model.creatures

  in
    walls ++ coins ++ creatures

wallView : Point -> Svg.Svg a
wallView pos =
  text' [ x (toString pos.x), y (toString pos.y), fontSize "1", fontFamily "Courier" ] [ Html.text "#" ]

coinView : Point -> Svg.Svg a
coinView pos =
  text' [ x (toString pos.x), y (toString pos.y), fontSize "1", fontFamily "Courier" ] [ Html.text "." ]
