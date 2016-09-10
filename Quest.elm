module Quest exposing (Quest, completed, describe, findCrystal, escape, goal, unlocked, coreCampaign)

import World
import Configuration

type Goal = FindWeapon
          | FindArmor
          | FindCrystal
          | Escape

type Quest = Quest Goal (List Quest)

coreCampaign : List Quest
coreCampaign =
  [ findWeapon ]

findWeapon : Quest
findWeapon =
  Quest FindWeapon [ findArmor ]

findArmor : Quest
findArmor =
  Quest FindArmor [ findCrystal ]

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

    FindArmor ->
      "Put on some armor"

    FindCrystal ->
      "Seek the Crystal"

    Escape ->
      "Escape the Halls"

completed : World.Model -> Quest -> Bool
completed world (Quest goal _) =
  case goal of
    FindCrystal ->
      world |> World.doesPlayerHaveCrystal

    Escape ->
      world.hallsEscaped

    FindWeapon ->
      not (world.player.weapon == Nothing)

    FindArmor ->
      not (world.player.armor == Nothing)

unlocked : World.Model -> List Quest -> List Quest
unlocked world quests =
  let
    (completed', incomplete) =
      quests
      |> List.partition (completed world)

    unlocked' =
      completed'
      |> List.concatMap (\(Quest _ unlocks) -> unlocks)
      |> List.filter (\(Quest goal' _) ->
        not (List.member goal' (List.map goal quests)))
  in
    unlocked'

