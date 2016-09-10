module Bresenham exposing (line)

import Point exposing (Point)

line : Point -> Point -> List Point
line (ax,ay) (bx,by) =
  if ax == bx then
    vline ax ay by
  else if ay == by then
    hline ay ax bx
  else
    line' (ax,ay) (bx,by)

vline : Int -> Int -> Int -> List Point
vline x y0 y1 =
  if y1 < y0 then 
    List.reverse (vline x y1 y0)
  else
    List.map (\y -> (x,y)) [y0..y1]

hline : Int -> Int -> Int -> List Point
hline y x0 x1 =
  if x1 < x0 then 
    List.reverse (hline y x1 x0) 
  else
    List.map (\x -> (x,y)) [x0..x1]

line' : Point -> Point -> List Point
line' (ax,ay) (bx,by) =
  let
    dy =
      toFloat (by - ay)
    dx =
      toFloat (bx - ax)
  in

  if abs dx > abs dy then
    let
      f = \x ->
        slope * toFloat (x - ax)

      slope =
        dy / dx
    in
      if ax > bx then 
        List.reverse (line' (bx,by) (ax,ay))
      else
        [ax..bx]
        |> List.map (\x -> (x, round (f x) + ay))
  else
    let
      f = \y ->
        slope * toFloat (y - ay)

      slope =
        dx / dy
    in
      if ay > by then 
        List.reverse (line' (bx,by) (ax,ay))
      else
        [(ay)..(by)]
        |> List.map (\y -> (round (f y) + ax, y))
