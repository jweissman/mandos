module Dungeon exposing (Dungeon, generate, moveCreatures, turnCreature, injureCreature, collectCoin, purge, levelAt)

import Warrior
import Creature
import Point exposing (Point)

import Room exposing (Room)
import Graph

import Direction exposing (Direction(..))

import Level exposing (Level)
import Event exposing (Event)

import Random
import Util

-- TYPE

type alias Dungeon = List Level

-- GENERATOR

generate : Int -> Random.Generator Dungeon
generate depth =
  Random.list depth (Random.map Level.fromRooms (Room.generate 1000))

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
  --model
  --|> levelAt depth
  --|> f
  f (levelAt depth model) -- |> f

collectCoin : Point -> Int -> Dungeon -> Dungeon
collectCoin pt depth model =
  model |> apply (Level.collectCoin pt) depth

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

turnCreature : Creature.Model -> Direction -> Int -> Dungeon -> Dungeon
turnCreature creature direction depth model =
  model |> apply (Level.turnCreature creature direction) depth

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
