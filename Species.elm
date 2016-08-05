module Species exposing (Species, name, glyph, hp, rat, monkey, bandit)

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

hp : Species -> Int
hp species =
  case species of
    Rat -> 10
    Snake -> 20
    Monkey -> 35
    Bandit -> 50
    Tiger -> 100
    Dragon -> 200
