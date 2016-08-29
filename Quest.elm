module Quest exposing (Quest, completed, describe, findCrystal, escape, goal, unlocked, coreCampaign)

import World

type Goal = FindWeapon
          --| FindArmor
          --| FindAlly
          | FindCrystal
          | Escape

type Quest = Quest Goal (List Quest)

coreCampaign : List Quest
coreCampaign =
  [ findWeapon
  --, findArmor
  --, findAlly
  ]

findWeapon : Quest
findWeapon =
  Quest FindWeapon [ findCrystal ]

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
    FindWeapon ->
      "Get a weapon"

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

    FindWeapon ->
      False

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
