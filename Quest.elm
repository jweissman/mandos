module Quest exposing (Quest, completed, describe, findCrystal, escape, goal, unlocked)

import World

type Goal = FindCrystal
          | Escape

type Quest = Quest Goal (List Quest)

--type Quest = Quest { goal : Goal
--                   , unlocks : (List Quest)
--                   }

findCrystal : Quest
findCrystal =
  Quest FindCrystal [ escape ]

escape : Quest
escape =
  Quest Escape []


goal : Quest -> Goal
goal (Quest goal' _) = goal'

describe : Quest -> String
describe (Quest goal _) =
  case goal of
    FindCrystal ->
      "Seek the Crystal of Time"

    Escape ->
      "Escape the Halls of Mandos"

completed : World.Model -> Quest -> Bool
completed world (Quest goal _) =
  case goal of
    FindCrystal ->
      world.crystalTaken

    Escape ->
      world.hallsEscaped


unlocked : World.Model -> List Quest -> List Quest
unlocked world quests =
  let
    completed' =
      quests
      |> List.filter (completed world)

    unlocked' =
      completed'
      |> List.concatMap (\(Quest _ unlocks) -> unlocks)
      |> List.filter (\(Quest goal' _) -> 
        not (List.member goal' (List.map goal quests)))

  in
    unlocked'

--findCrystal =
--  FindCrystal
--
--escape =
--  Escape
