module Event exposing (Event, awakenEvent, pickupCoinEvent, attackEvent, killEnemyEvent, defendEvent, describeEvent)

import Creature

-- TYPES

type Event
  = Awaken
  | PickupCoin
  | AttackEnemy Creature.Model Int
  | KillEnemy Creature.Model
  | DefendEnemy Creature.Model Int

-- ctors
awakenEvent =
  Awaken

pickupCoinEvent =
  PickupCoin

attackEvent target damage =
  AttackEnemy target damage

killEnemyEvent target =
  KillEnemy target

defendEvent target damage =
  DefendEnemy target damage

-- helpers
describeEvent : Event -> String
describeEvent event =
  case event of
    Awaken -> "You awaken in the Timeless Halls of Mandos..."
    PickupCoin -> "You find a glittering golden coin."
    AttackEnemy enemy dmg -> "You attack the " ++ (Creature.describe enemy) ++ " for " ++ (toString dmg) ++ " damage."
    KillEnemy enemy -> "You slay the " ++ (enemy.name) ++ "!"
    DefendEnemy enemy dmg -> "You were attacked by the " ++ (Creature.describe enemy) ++ " for " ++ (toString dmg) ++ " damage."
