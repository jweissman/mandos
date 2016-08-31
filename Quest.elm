module Quest exposing (Quest, completed, describe, findCrystal, escape, goal, unlocked, coreCampaign)

import World

type Goal = FindWeapon
          | FindArmor
          | FindCrystal
          | Escape
          | DescendLevels Int
          | CompleteSubquests String (List Quest)

type Quest = Quest Goal (List Quest)

coreCampaign : List Quest
coreCampaign =
  [ getReady, getDown ]

getReady : Quest
getReady =
  let quests = (CompleteSubquests "Get ready" [ findWeapon, findArmor ]) in
  Quest quests [ ]

getDown : Quest
getDown =
  Quest (DescendLevels 5) [ findCrystal ]

findWeapon : Quest
findWeapon =
  Quest FindWeapon []

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
      "Seek the Crystal of Time"

    Escape ->
      "Escape the Halls of Mandos"

    DescendLevels n ->
      "Descend " ++ toString n ++ " levels"

    CompleteSubquests desc _ ->
      desc

completed : World.Model -> Quest -> Bool
completed world (Quest goal _) =
  case goal of
    FindCrystal ->
      world.crystalTaken

    Escape ->
      world.hallsEscaped

    FindWeapon ->
      not (world.player.weapon == Nothing)

    FindArmor ->
      not (world.player.armor == Nothing)

    DescendLevels depth ->
      world.depth > depth

    CompleteSubquests _ qs ->
      List.all (completed world) qs

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

    subquests =
      incomplete
      |> List.concatMap (\(Quest kind _) ->
        case kind of
          CompleteSubquests _ qs ->
            qs
          _ -> []
        )
      |> List.filter (\(Quest goal' _) ->
        not (List.member goal' (List.map goal quests)))
  in
    unlocked' ++ subquests
