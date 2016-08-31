module Journal exposing (Journal, view)

import Point exposing (Point)
import Quest exposing (Quest)
import World
import Graphics

import Svg

type alias Journal = List Quest

view : Point -> World.Model -> List Quest -> List (Svg.Svg a)
view (x,y) world quests = 
  let
    (completed, active) =
      quests
      |> List.partition (Quest.completed world)

    title =
      (Graphics.render "QUESTS" (x,y) "grey")
  in
    title ::
      (questGroupView (x,y+1) active "[ ]" "lightgrey") ++
      (questGroupView (x,y+1+(List.length active)) completed "[x]" "darkgrey")

questGroupView (x,y) quests prefix color =
  (quests
  |> List.indexedMap (\idx q ->
    Graphics.render (prefix ++ " " ++ Quest.describe q) (x,y+1+idx) color))
