module Journal exposing (Journal, view)

import Point exposing (Point)
import Quest exposing (Quest)
import World
import Graphics
import Palette

import Svg

type alias Journal = List Quest

view : Point -> World.Model -> List Quest -> List (Svg.Svg a)
view (x,y) world quests = 
  let
    (completed, active) =
      quests
      |> List.partition (Quest.completed world)

    completed' =
      quests

    title =
      (Graphics.render "QUESTS" (x,y) Palette.tertiaryLighter)
  in
    title ::
      (questGroupView (x,y+1) active "[ ]" Palette.active) ++
      (questGroupView (x,y+1+(List.length active)) completed "[x]" Palette.inactive)

questGroupView (x,y) quests prefix color =
  (quests
  |> List.indexedMap (\idx q ->
    Graphics.render (prefix ++ " " ++ Quest.describe q) (x,y+1+idx) color))
