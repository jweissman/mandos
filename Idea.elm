module Idea exposing (Idea, describe, ideas, light, power, water, compound, holy, life)

import String

type Idea = Power
          | Light
          | Life
          | Water
          | Holy
          --| Imagination
          | Compound (List Idea)

light =
  Light

life =
  Life

power =
  Power

water =
  Water

holy =
  Holy

compound is =
  Compound is

ideas =
  [ Power
  , Light
  , Life
  , Water
  , Holy
  , Compound [Holy, Water]
  ]

describe : Idea -> String
describe idea =
  case idea of
    Power ->
      "power"

    Light ->
      "light"

    Life ->
      "life"

    Water ->
      "water"

    Holy ->
      "holy"

    Compound ideas ->
      ideas
      |> List.map describe
      |> String.join " "
