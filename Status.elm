module Status exposing (view)

import World
import Graphics
import Point exposing (Point)

import String
import Svg

view : Point -> World.Model -> Svg.Svg a
view (x,y) model =
  let
    mandos =
      "MANDOS v0.1"

    level =
      "LEVEL: " ++ toString model.depth

    gold =
      "GOLD: " ++ toString model.player.gold

    hp =
      "HP: " ++ toString model.player.hp ++ "/" ++ toString model.player.maxHp

    parts =
      [ mandos
      , gold
      , hp
      , level
      ]

    message =
      String.join "  |  " parts

  in
     Graphics.render message (x,y) "lightgray"
