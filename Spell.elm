module Spell exposing (Spell(..), idea, lux, infuse)

import Idea exposing (Idea)

type Spell = Lux
           | Infuse

lux =
  Lux

infuse =
  Infuse

--describe : Spell -> String
--describe spell =
--  case spell of
--    Lux ->
--      "lux"
--
--    Infuse ->
--      "infuse"

idea : Spell -> Idea
idea spell =
  case spell of
    Lux ->
      Idea.light

    Infuse ->
      Idea.power
