module Util exposing (directionBetween, simpleDirectionBetween, minBy, uniqueBy, getAt)

import Point exposing (Point)
import Direction exposing (Direction(..))
import Set exposing (Set)

directionBetween : Point -> Point -> Direction
directionBetween a b =
  if (a.x > b.x) && (a.y > b.y) then
     Southeast
  else
    if (a.x < b.x) && (a.y > b.y) then
      Southwest
    else
      if (a.x > b.x) && (a.y < b.y) then
        Northeast
      else
        if (a.x < b.x) && (a.y < b.y) then
          Northwest
        else
          simpleDirectionBetween a b

simpleDirectionBetween : Point -> Point -> Direction
simpleDirectionBetween a b =
  let
    dx =
      abs (a.x - b.x)
    dy =
      abs (a.y - b.y)

  in
    if dx > dy then
      if (a.x > b.x) then
        East
      else
        West
    else
      if (a.y > b.y) then
        South
      else
        North

-- from list extras
{-| Find the first minimum element in a list using a comparable transformation
-}
minBy : (a -> comparable) -> List a -> Maybe a
minBy f ls =
  let minBy x (y, fy) = let fx = f x in if fx < fy then (x, fx) else (y, fy)
  in case ls of
        [l']    -> Just l'
        l'::ls' -> Just <| fst <| List.foldl minBy (l', f l') ls'
        _       -> Nothing

uniqueBy f list =
  uniqueHelp f Set.empty list

uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a
uniqueHelp f existing remaining =
  case remaining of
    [] ->
      []

    first :: rest ->
      let computedFirst = f first in
      if Set.member computedFirst existing then
        uniqueHelp f existing rest
      else
        first :: uniqueHelp f (Set.insert computedFirst existing) rest

getAt : List a -> Int -> Maybe a
getAt xs idx = List.head <| List.drop idx xs



