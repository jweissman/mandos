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
    Monkey -> 3
    Snake  -> 4
    Bandit -> 5
    Tiger  -> 6
    Dragon -> 10

resistance : Species -> Int
resistance species =
  case species of
    Rat    -> 2
    Snake  -> 3
    Monkey -> 4
    Bandit -> 5
    Tiger  -> 7
    Dragon -> 10

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
    Rat -> 5
    Snake -> 7
    Monkey -> 10
    Bandit -> 14
    Tiger -> 25
    Dragon -> 50
