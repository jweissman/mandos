module ChallengeRating exposing (ChallengeRating(..), forDepth)

import Configuration exposing (levelCount)

type ChallengeRating = Beginner
                     | Easy
                     | Moderate
                     | Hard
                     | Impossible


forDepth : Int -> ChallengeRating
forDepth level =
  if level == 0 then
    Beginner
  else if level < levelCount // 4 then
    Easy
  else if level < levelCount // 2 then
    Moderate
  else if level < 3 * (levelCount // 4) then
    Hard
  else
    Impossible
