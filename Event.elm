module Event exposing (Event, awaken, pickupCoin, attack, killEnemy, defend, describe)

import Creature

-- TYPES

type Event
  = Awaken
  | PickupCoin
  | AttackEnemy Creature.Model Int
  | KillEnemy Creature.Model
  | DefendEnemy Creature.Model Int

-- ctors
awaken =
  Awaken

pickupCoin =
  PickupCoin

attack target damage =
  AttackEnemy target damage

killEnemy target =
  KillEnemy target

defend target damage =
  DefendEnemy target damage

-- helpers
describe : Event -> String
describe event =
  case event of
    Awaken -> 
      "You awaken in the Timeless Halls of Mandos..."

    PickupCoin -> 
      "You find a glittering golden coin."

    AttackEnemy enemy dmg -> 
      "You attack " ++ (Creature.describe enemy) ++ " for " ++ (toString dmg) ++ " damage."

    KillEnemy enemy -> 
      "You slay " ++ (Creature.describe enemy) ++ "!"

    DefendEnemy enemy dmg -> 
      "You were attacked by " ++ (Creature.describe enemy) ++ " for " ++ (toString dmg) ++ " damage."
