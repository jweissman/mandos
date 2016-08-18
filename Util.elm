module Util exposing (directionBetween)

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
        if (a.x < b.x) && (a.y > b.y) then
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

