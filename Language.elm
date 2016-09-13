module Language exposing (Language, Word, generate, decode, wordFor)

import Util
import Idea exposing (Idea)

import Random exposing (Generator)
import String

type Word = Root Idea String

type alias Language = List Word

syllables =
  [ "ae"
  , "al"
  , "au"
  , "ch"
  , "de"
  , "ea"
  , "el"
  , "en"
  , "ep"
  , "es"
  , "eu"
  , "jo"
  , "li"
  , "ll"
  , "lm"
  , "lo"
  , "ma"
  , "mne"
  , "mu"
  , "no"
  , "nu"
  , "oe"
  , "oh"
  , "oi"
  , "or"
  , "ru"
  , "ry"
  , "sa"
  , "sho"
  , "thi"
  , "us"
  ]

-- init

init : Idea -> String -> Word
init idea description =
  Root idea description

secret =
  Root Idea.holy "???"

-- translation
decode : Idea -> Language -> Language -> String
decode idea known model =
  let
    knownIdea =
      known
      |> List.any (\(Root idea' _) -> idea == idea')
  in if knownIdea then
    Idea.describe idea
  else
    model
    |> foreignWordFor idea

foreignWordFor : Idea -> Language -> String
foreignWordFor idea model =
  model
  |> List.filter (\(Root idea' _) -> idea == idea')
  |> List.map (\(Root _ word) -> word)
  |> List.head
  |> Maybe.withDefault "???"

wordFor : Idea -> Language -> Word
wordFor idea model =
  model
  |> List.filter (\(Root idea' _) -> idea == idea')
  |> List.head
  |> Maybe.withDefault secret

-- generation

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
  Random.list (List.length Idea.ideas) generateWord

generate : Generator Language
generate =
  let
    expression = \words ->
      List.map2 init Idea.ideas words
  in
    Random.map expression generateWords
