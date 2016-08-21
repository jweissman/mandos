module Event exposing (Event, awaken, pickupCoin, attack, killEnemy, defend, enemyEngaged, death, describe)

import Creature

-- TYPES

type Event
  = Awaken
  | PickupCoin
  | EnemyEngaged Creature.Model
  | AttackEnemy Creature.Model Int
  | KillEnemy Creature.Model
  | DefendEnemy Creature.Model Int
  | Death

-- ctors
awaken =
  Awaken

pickupCoin =
  PickupCoin

enemyEngaged enemy =
  EnemyEngaged enemy

attack target damage =
  AttackEnemy target damage

killEnemy target =
  KillEnemy target

defend target damage =
  DefendEnemy target damage

death =
  Death

-- helpers
describe : Event -> String
describe event =
  case event of
    Awaken -> 
      "You awaken in the Timeless Halls of Mandos..."

    PickupCoin -> 
      "You find a glittering golden coin."

    EnemyEngaged enemy ->
      (Creature.describe enemy) ++ " engages you!"

    AttackEnemy enemy dmg -> 
      "You attack " ++ (Creature.describe enemy) ++ " for " ++ (toString dmg) ++ " damage."

    KillEnemy enemy -> 
      "You slay " ++ (Creature.describe enemy) ++ "!"

    DefendEnemy enemy dmg -> 
      "You are attacked by " ++ (Creature.describe enemy) ++ " for " ++ (toString dmg) ++ " damage."

    Death ->
      "You were slain..."


