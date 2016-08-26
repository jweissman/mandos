module Bresenham exposing (line)

import Point exposing (Point)
import Path exposing (Path)

line : Point -> Point -> Path
line src dst =
  let
    dy =
      toFloat (dst.y - src.y)

    dx =
      toFloat (dst.x - src.x)
  in
    if dx == 0 then
      vline src.x src.y dst.y
    else if dy == 0 then
      hline src.y src.x dst.x
    else
      line' src dst (dx,dy) 

vline x y0 y1 =
  if y1 < y0 then List.reverse (vline x y1 y0) else
  List.map (\y -> {x=x,y=y}) [y0..y1]
    
hline y x0 x1 =
  if x1 < x0 then List.reverse (hline y x1 x0) else
  List.map (\x -> {x=x,y=y}) [x0..x1]

line' src dst (dx,dy) =
  if abs dx > abs dy then
    let 
      f = \x -> 
        slope * toFloat (x - src.x) 

      slope = 
        dy / dx
    in
      if src.x > dst.x then (List.reverse (line dst src)) else
      [(src.x)..(dst.x)]
      |> List.map (\x -> {x=x, y=round (f x) + src.y})
  else
    let 
      f = \y -> 
        slope * toFloat (y - src.y)
          
      slope = 
        dx / dy
    in
      if src.y > dst.y then (List.reverse (line dst src)) else
      [(src.y)..(dst.y)]
      |> List.map (\y -> {x=round (f y) + src.x, y=y})


