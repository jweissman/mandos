module Util exposing (directionBetween, minBy)

import Point exposing (Point)
import Direction exposing (Direction(..))

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
  if (a.x > b.x) then
    East
  else
    if (a.x < b.x) then
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
