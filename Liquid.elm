module Liquid exposing (Liquid(..), Effect(..), idea, water, lifePotion, holyWater)

import Idea exposing (Idea)

type Effect = GainLife

type Liquid = Water
            | Blessed Liquid
            | Potion Effect

water =
  Water

holy liquid =
  Blessed liquid

lifePotion =
  Potion GainLife

holyWater =
  Blessed Water

idea : Liquid -> Idea
idea liquid =
  case liquid of
    Water ->
      Idea.water

    Blessed liquid' ->
      Idea.compound [Idea.holy, (idea liquid')]

    Potion effect ->
      case effect of
        GainLife ->
          Idea.life
