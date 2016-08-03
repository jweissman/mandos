module Rogue.Log exposing (Model, init, view, awakenEvent, pickupCoinEvent, attackEvent, killEnemyEvent, defendEvent)

import Rogue.Creature

import Html
import Svg exposing (text')
import Svg.Attributes exposing (x,y,fontSize,fontFamily)


-- TYPES

type Event = Awaken | PickupCoin | AttackEnemy Rogue.Creature.Model Int | KillEnemy Rogue.Creature.Model | DefendEnemy Rogue.Creature.Model Int

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

-- MODEL
type alias Model = List Event


-- INIT
init : Model
init = [ awakenEvent ]


-- VIEW
view : Model -> List (Svg.Svg a)
view model =
  let
    notes =
      List.take 5 (List.reverse (List.map eventMessage model))

    logLines =
      List.indexedMap logLineView notes

    header =
      text' [ x "15", y "2", fontSize "1", fontFamily "Courier" ] [ Html.text "EVENT LOG:" ]

  in
    [header] ++ logLines

logLineView : Int -> String -> Svg.Svg a
logLineView idx note =
  text' [ x "15", y (toString (3+idx)), fontSize "1", fontFamily "Courier" ] [ Html.text note ]


eventMessage : Event -> String
eventMessage event =
  case event of
    Awaken -> "You awaken in the Timeless Halls of Mandos..."
    PickupCoin -> "You find a glittering golden coin."
    AttackEnemy enemy dmg -> "You attack the " ++ (Rogue.Creature.describe enemy) ++ " for " ++ (toString dmg) ++ " damage."
    KillEnemy enemy -> "You slay the " ++ (enemy.name) ++ "!"
    DefendEnemy enemy dmg -> "You were attacked by the " ++ (Rogue.Creature.describe enemy) ++ " for " ++ (toString dmg) ++ " damage."
