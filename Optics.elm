module Optics exposing (illuminate)

import Set exposing (Set)
import Point exposing (Point, code)
import Util
import Bresenham exposing (line)
import Configuration

illuminate : List Point -> Set Point -> Point -> List Point
illuminate perimeter blockers source =
  let
    power =
      Configuration.visionRadius

    rays =
      castRay power blockers source
  in
    perimeter
    |> List.concatMap rays
    |> Util.uniqueBy Point.code

castRay : Int -> Set Point -> Point -> Point -> List Point
castRay power blockers src dst =
  let
    line' =
      line src dst
      |> List.tail
      |> Maybe.withDefault []

  in
    line'
    |> Util.takeWhile' (\pt -> not (Set.member pt blockers) && (Point.distance src pt) < toFloat power)
