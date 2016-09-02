module Species exposing (Species, name, adjective, glyph, hp, power, resistance, level)

-- TYPE
type Species = Bandit | Rat | Snake | Tiger | Dragon | Monkey

-- ctors

level : Int -> List Species
level depth =
  if depth < 1 then
    [ Rat, Rat, Rat, Rat ]
  else if depth < 3 then
    [ Rat, Monkey, Rat, Snake ]
  else if depth < 5 then
    [ Monkey, Bandit, Snake, Monkey ]
  else if depth < 7 then
    [ Bandit, Tiger, Snake, Bandit ]
  else if depth < 9 then
    [ Tiger, Bandit, Tiger, Bandit ]
  else
    [ Dragon, Dragon, Dragon, Dragon ]

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


