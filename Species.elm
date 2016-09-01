module Species exposing (Species, name, adjective, glyph, hp, power, resistance, rat, monkey, bandit)

-- TYPE
type Species = Bandit | Rat | Snake | Tiger | Dragon | Monkey

-- ctors

rat = Rat
monkey = Monkey
bandit = Bandit

-- helpers

power : Species -> Int
power species =
  case species of
    Rat    -> 2
    Monkey -> 3
    Bandit -> 4
    Tiger  -> 8
    Snake  -> 7
    Dragon -> 12

resistance : Species -> Int
resistance species =
  case species of
    Rat    -> 1
    Monkey -> 2
    Bandit -> 3
    Tiger  -> 4
    Snake  -> 5
    Dragon -> 7


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
    Monkey -> 4
    Bandit -> 7
    Snake -> 14
    Tiger -> 25
    Dragon -> 46
