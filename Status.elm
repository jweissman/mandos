module Status exposing (view)

import World
import Graphics
import Point exposing (Point)
import Palette

import String
import Svg

view : Point -> World.Model -> List (Svg.Svg a)
view (x,y) model =
  let
    level =
      levelView (x,y) model.depth

    gold =
      goldView (x+5,y) model.player.gold

    life =
      lifeView (x+10,y) model.player.hp model.player.maxHp
  in
    level
    ++ gold
    ++ life

levelView : Point -> Int -> List (Svg.Svg a)
levelView (x,y) depth =
  [ Graphics.render "LEVEL" (x,y) Palette.primaryLighter
  , Graphics.render (toString (depth+1)) (x+3,y) Palette.bright
  ]

goldView : Point -> Int -> List (Svg.Svg a)
goldView (x,y) amt =
  [ Graphics.render "GOLD" (x,y) Palette.secondaryLighter
  , Graphics.render ((toString amt) ++ "p") (x+3,y) Palette.bright
  ]

lifeView : Point -> Int -> Int -> List (Svg.Svg a)
lifeView (x,y) hp maxHp =
  [ Graphics.render "LIFE" (x,y) Palette.accentLighter
  , Graphics.render (toString hp) (x+3,y) Palette.bright
  , Graphics.render "/" (x+4,y) Palette.bright
  , Graphics.render (toString maxHp) (x+5,y) Palette.bright
  ]
