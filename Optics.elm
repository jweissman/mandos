module Optics exposing (illuminate)

import Point exposing (Point, code)
import Util
import Bresenham exposing (line)

illuminate : List Point -> List Point -> Point -> List Point
illuminate perimeter blockers source =
  let
    rays =
      castRay blockers source
  in
    perimeter
    |> List.concatMap rays
    |> Util.uniqueBy Point.code

castRay : List Point -> Point -> Point -> List Point
castRay blockers src dst =
  let
    line' =
      line src dst
      |> List.tail
      |> Maybe.withDefault []

  in
    line'
    |> Util.takeWhile' (\pt -> not (List.member pt blockers))
