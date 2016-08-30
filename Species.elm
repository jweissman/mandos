module Species exposing (Species, name, adjective, glyph, hp, rat, monkey, bandit)

-- TYPE
type Species = Bandit | Rat | Snake | Tiger | Dragon | Monkey

-- ctors

rat = Rat
monkey = Monkey
bandit = Bandit

-- helpers

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
    Rat -> 2
    Snake -> 4
    Monkey -> 5
    Bandit -> 7
    Tiger -> 10
    Dragon -> 16
