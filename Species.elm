module Species exposing (Species, name, adjective, glyph, hp, power, resistance, level)

import ChallengeRating exposing (ChallengeRating(..))

-- TYPE
type Species = Bandit | Rat | Snake | Tiger | Dragon | Monkey

-- ctors

level : ChallengeRating -> List Species
level rating =
  case rating of
    Beginner ->
      [ Rat, Rat, Rat, Rat, Rat ]

    Easy ->
      [ Rat, Monkey, Rat, Snake, Rat ]

    Moderate ->
      [ Monkey, Bandit, Snake, Monkey, Monkey ]

    Hard ->
      [ Bandit, Tiger, Snake, Bandit, Monkey ]

    Impossible ->
      [ Dragon, Tiger, Dragon, Dragon, Tiger ]

-- helpers
power : Species -> Int
power species =
  case species of
    Rat    -> 2
    Snake  -> 7
    Monkey -> 10
    Bandit -> 18
    Tiger  -> 30
    Dragon -> 40

resistance : Species -> Int
resistance species =
  case species of
    Rat    -> 2
    Snake  -> 4
    Monkey -> 7
    Bandit -> 9
    Tiger  -> 15
    Dragon -> 26

glyph : Species -> Char
glyph species =
  case species of
    Bandit -> 'b'
    Rat    -> 'r'
    Snake  -> 's'
    Tiger  -> 't'
    Dragon -> 'd'
    Monkey -> 'm'

name : Species -> String
name species =
  case species of
    Bandit -> "bandit"
    Rat    -> "rat"
    Snake  -> "snake"
    Tiger  -> "tiger"
    Dragon -> "drake"
    Monkey -> "monkey"

adjective : Species -> String
adjective species =
  case species of
    Bandit -> "ruthless"
    Rat    -> "vicious"
    Snake  -> "wild"
    Tiger  -> "savage"
    Dragon -> "cruel"
    Monkey -> "angry"

hp : Species -> Int
hp species =
  case species of
    Rat -> 6
    Snake -> 10
    Monkey -> 14
    Bandit -> 20
    Tiger -> 45
    Dragon -> 150


