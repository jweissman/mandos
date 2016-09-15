module Optics exposing (illuminate, castRay)

import Set exposing (Set)
import Point exposing (Point, code)
import Util
import Bresenham exposing (line)
import Configuration

illuminate : Int -> List Point -> Set Point -> Point -> Set Point
illuminate power perimeter blockers source =
  let
    rays =
      castRay power blockers source
  in
    perimeter
    |> List.concatMap rays
    |> Set.fromList

castRay : Int -> Set Point -> Point -> Point -> List Point
castRay power blockers src dst =
  let
    pts =
      line src dst
      |> List.tail
      |> Maybe.withDefault []

    notAbsorbed = \pt ->
      not (Set.member pt blockers)
      && not ((Point.distance src pt) > toFloat power)

  in
    pts
    |> takeWhile' notAbsorbed

takeWhile' : (a -> Bool) -> List a -> List a
takeWhile' predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then x :: takeWhile' predicate xs
               else [x]
