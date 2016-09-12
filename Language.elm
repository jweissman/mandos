module Language exposing (Language, Word, Idea, generate)

import Util

import Random exposing (Generator)
import String

type Idea = Power
          | Light
          | Life
          | Water
          | Holy
          --| Imagination
          | Compound (List Idea)

type Word = Root Idea String

type alias Language = List Word

describeIdea : Idea -> String
describeIdea idea =
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
      |> List.map describeIdea
      |> String.join " "

ideas =
  [ Power
  , Light
  , Life
  , Water
  , Holy
  --, Compound [Holy, Water]
  ]

syllables =
  [ "ae"
  , "au"
  , "ch"
  , "de"
  , "ea"
  , "es"
  , "eu"
  , "en"
  , "li"
  , "ll"
  , "ma"
  , "mu"
  , "mn"
  , "no"
  , "nu"
  , "oi"
  , "oh"
  , "ru"
  , "sa"
  , "sh"
  , "th"
  , "us"
  ]


init : Idea -> String -> Word
init idea description =
  Root idea description

generateSyllable : Generator String
generateSyllable =
  let
    pickSyllable = \idx ->
      Util.getAt syllables idx
      |> Maybe.withDefault "zz"

    randomIdx =
      Random.int 0 (List.length syllables-1)
  in
    Random.map pickSyllable randomIdx

generateWord : Generator String
generateWord =
  let
    randomCount =
      Random.int 2 3

    syllableList =
      Random.list 10 generateSyllable

    constructWord = \ls ct -> 
      ls 
      |> List.take ct 
      |> String.join ""
  in
    Random.map2 constructWord syllableList randomCount

generateWords : Generator (List String)
generateWords =
  Random.list (List.length ideas) generateWord

generate : Generator Language
generate =
  let
    expression = \words -> 
      List.map2 init ideas words
  in
    Random.map expression generateWords
