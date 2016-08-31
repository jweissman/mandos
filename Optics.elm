module Optics exposing (illuminate)

import Set exposing (Set)
import Point exposing (Point, code)
import Util
import Bresenham exposing (line)

illuminate : List Point -> Set Point -> Point -> List Point
illuminate perimeter blockers source =
  let
    rays =
      castRay blockers source
  in
    perimeter
    |> List.concatMap rays
    |> Util.uniqueBy Point.code

castRay : Set Point -> Point -> Point -> List Point
castRay blockers src dst =
  let
    line' =
      line src dst
      |> List.tail
      |> Maybe.withDefault []

  in
    line'
    |> Util.takeWhile' (\pt -> not (Set.member pt blockers))
