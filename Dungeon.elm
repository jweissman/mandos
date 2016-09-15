module Dungeon exposing (Dungeon, generate, prepare, moveCreatures, injureCreature, collectCoin, purge, levelAt, playerSees, removeItem, playerDestroysWall, evolve, viewFrontier, apply)

import Warrior
import Creature
import Point exposing (Point)
import Room exposing (Room)
import Graph
import Direction exposing (Direction(..))
import Level exposing (Level)
import Event exposing (Event)
import Util
import Item exposing (Item)
import Configuration

import Random
import Set exposing (Set)

-- TYPE
type alias Dungeon = List Level

-- GENERATOR
generate : Int -> Random.Generator Dungeon
generate depth =
  Random.list (depth) (Random.map Level.fromRooms (Room.generate Configuration.candidateRooms))

prepare : Int -> Dungeon -> Dungeon
prepare depth model =
  model
  |> List.indexedMap Level.finalize

-- HELPERS
levelAt : Int -> Dungeon -> Level
levelAt depth model =
  Util.getAt model depth
  |> Maybe.withDefault Level.init

apply : (Level -> Level) -> Int -> Dungeon -> Dungeon
apply f depth model =
  let
    level =
      model |> apply' f depth

    model' =
      model |> List.indexedMap (\n level' ->
        if n == depth then level else level')
  in
    model'

apply' : (Level -> a) -> Int -> Dungeon -> a
apply' f depth model =
  f (levelAt depth model)

collectCoin : Point -> Int -> Dungeon -> Dungeon
collectCoin pt depth model =
  model
  |> apply (Level.collectCoin pt) depth

removeItem : Item -> Int -> Dungeon -> Dungeon
removeItem item depth model =
  model
  |> apply (Level.removeItem item) depth

moveCreatures : Warrior.Model -> Int -> Dungeon -> (Dungeon, List Event, Warrior.Model)
moveCreatures player depth model =
  let
    (level, events, player') =
      model |> apply' (Level.moveCreatures player) depth

    model' =
      model |> List.indexedMap (\n level' ->
        if n == depth then level else level')
  in
    (model', events, player')

injureCreature : Creature.Model -> Int -> Int -> Dungeon -> Dungeon
injureCreature creature amount depth model =
  model |> apply (Level.injureCreature creature amount) depth

purge : Int -> Dungeon -> (Dungeon, List Event)
purge depth model =
  let
    (level, events) =
      model |> apply' Level.purge depth
    model' =
      model |> List.indexedMap (\n level' ->
        if n == depth then level else level')
  in
    (model', events)

playerSees : Set Point -> Int -> Dungeon -> Dungeon
playerSees pts depth model =
  model |> apply (Level.playerSees pts) depth

playerDestroysWall : Point -> Int -> Dungeon -> Dungeon
playerDestroysWall pt depth model =
  model |> apply (Level.extrude pt) depth

evolve : Dungeon -> Dungeon
evolve model =
  model
  |> List.map (\level -> level |> Level.evolveGrass 1)

viewFrontier : Int -> Dungeon -> Set Point
viewFrontier depth model =
  model |> apply' (Level.viewFrontier) depth
