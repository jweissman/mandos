module ChallengeRating exposing (ChallengeRating(..), forDepth)

import Configuration exposing (levelCount)

type ChallengeRating = Beginner
                     | Easy
                     | Moderate
                     | Hard
                     | Impossible


forDepth : Int -> ChallengeRating
forDepth level =
  if level < levelCount // 5 then
    Beginner
  else if level < 2 * levelCount // 5 then
    Easy
  else if level < 3 * levelCount // 5 then
    Moderate
  else if level < 4 * (levelCount // 5) then
    Hard
  else
    Impossible
