module Warrior exposing (Model, init, step, takeDamage, enrich, collectsItem, drink, wield, wear, computeDamageAgainst, resistance, cardView, augmentVision, itemAtIndex, sheatheWeapon, takeOffArmor)

import Configuration
import Util
import Direction exposing (Direction(..))
import Point exposing (Point, slide)
import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Item exposing (Item, ItemKind(..))
import Action exposing (Action)
import Liquid exposing (Liquid(..))

import Graphics
import Svg

-- MODEL

type alias Model =
  { hp : Int
  , maxHp : Int
  , direction : Direction
  , position : Point
  , gold : Int
  , attack : Int
  , defense : Int
  , steps : Int
  , weapon : Maybe Weapon
  , armor : Maybe Armor
  , inventory : List Item
  , timesGearChanged : Int
  , visionRadius : Int
  }

-- INIT

init : Point -> Model
init point =
  let hp = Configuration.startingHitPoints in
  { hp = hp
  , maxHp = hp
  , direction = North
  , position = point
  , gold = 0
  , attack = 1
  , defense = 1
  , steps = 0
  , weapon = Nothing
  , armor = Nothing
  , inventory = []
  , timesGearChanged = 0
  , visionRadius = Configuration.visionRadius
  }

power : Model -> Int
power model =
  case model.weapon of
    Nothing ->
      model.attack
    Just weapon ->
      model.attack + (Weapon.averageDamage weapon)

resistance : Model -> Int
resistance model =
  case model.armor of
    Nothing ->
      model.defense
    Just armor ->
      model.defense + (Armor.absorption armor)

-- helpers
step : Direction -> Model -> Model
step direction model =
  let model' = { model | position = model.position |> slide direction
                       , steps = model.steps + 1
                       }
  in
    if model.steps % 10 == 0 then
      model' |> heal 1
    else
      model'

computeDamageAgainst : Int -> Model -> Int
computeDamageAgainst defense model =
  let
    damage = case model.weapon of
      Just weapon ->
        model.attack + Weapon.damage model.steps model.timesGearChanged weapon
      Nothing ->
        model.attack
  in
    max 1 (damage - defense)

takeDamage : Int -> Model -> Model
takeDamage amount model =
  { model | hp = model.hp - amount }

enrich : Int -> Model -> Model
enrich amount model =
  { model | gold = model.gold + amount }

heal : Int -> Model -> Model
heal amount model =
  { model | hp = min model.maxHp (model.hp + amount) }

augmentVision : Int -> Model -> Model
augmentVision amount model =
  { model | visionRadius = model.visionRadius + amount }

drink : Liquid -> Model -> Model
drink liquid model =
  case liquid of
    Liquid.Water ->
      model
      |> heal 5

    Liquid.Blessed liquid' ->
      model
      |> heal 10
      |> drink liquid'

wield : Weapon -> Model -> Model
wield weapon model =
  case model.weapon of
    Just weapon' ->
      { model | weapon = Just weapon
              , inventory = model.inventory ++ [Item.init (0,0) (Item.weapon weapon') (1000000 + model.timesGearChanged)]
              , timesGearChanged = model.timesGearChanged + 1
      }

    Nothing ->
      { model | weapon = Just weapon }


sheatheWeapon : Model -> Model
sheatheWeapon model =
  case model.weapon of
    Nothing ->
      model

    Just weapon ->
      { model | weapon = Nothing
              , inventory = model.inventory ++ [Item.init (0,0) (Item.weapon weapon) (1000000 + model.timesGearChanged)]
              , timesGearChanged = model.timesGearChanged + 1
            }


takeOffArmor : Model -> Model
takeOffArmor model =
  case model.armor of
    Nothing ->
      model

    Just armor ->
      { model | armor = Nothing
              , inventory = model.inventory ++ [Item.init (0,0) (Item.armor armor) (1000000 + model.timesGearChanged)]
              , timesGearChanged = model.timesGearChanged + 1
            }

wear : Armor -> Model -> Model
wear armor model =
  case model.armor of
    Just armor' ->
      { model | armor = Just armor
              , inventory = model.inventory ++ [Item.init (0,0) (Item.armor armor') (1000000 + model.timesGearChanged)]
              , timesGearChanged = model.timesGearChanged + 1
      }

    Nothing ->
      { model | armor = Just armor }

collectsItem : Item -> Model -> Model
collectsItem item model =
  let
    {kind} =
      item

    model' =
      { model | inventory = model.inventory ++ [item] }
  in case kind of
    Arm weapon ->
      case model.weapon of
        Nothing ->
          model |> wield weapon
        Just weapon' ->
          if (Weapon.averageDamage weapon' < Weapon.averageDamage (weapon)) then
             model |> wield weapon
          else
            model'

    Shield armor ->
      case model.armor of
        Nothing ->
          model |> wear armor
        Just armor' ->
          if (Armor.absorption armor' < Armor.absorption (armor)) then
            model |> wear armor
          else
            model'

    _ ->
      model'

itemAtIndex : Int -> Model -> Maybe Item
itemAtIndex idx model =
  case model.weapon of
    Just weapon' ->
      case model.armor of
        Just armor' ->
          if idx == 0 then
            Just (Item.simple (Item.weapon weapon'))
          else if idx == 1 then
            Just (Item.simple (Item.armor armor'))
          else
            Util.getAt model.inventory (idx-2)

        Nothing ->
          if idx == 0 then
            Just (Item.simple (Item.weapon weapon'))
          else
            Util.getAt model.inventory (idx-1)

    Nothing ->
      case model.armor of
        Just armor' ->
          if idx == 0 then
            Just (Item.simple (Item.armor armor'))
          else
            Util.getAt model.inventory (idx-1)

        Nothing ->
          Util.getAt model.inventory idx

-- VIEW
cardView : Point -> Maybe Action -> Model -> List (Svg.Svg a)
cardView (x,y) action model =
  let
    strength =
      toString (power model)

    resist =
      toString (resistance model)

    stats =
      [ Graphics.render "STATS" (x, y) "gray"
      , Graphics.render ("  STRENGTH: " ++ strength) (x, y+2) "lightgray"
      , Graphics.render ("RESISTANCE: " ++ resist) (x, y+3) "lightgray"
      ]

    inventory =
      inventoryView (x,y+7) action model
  in
    stats
    ++ inventory

inventoryView : Point -> Maybe Action -> Model -> List (Svg.Svg a)
inventoryView (x,y) action model =
  let
    header =
      [ Graphics.render "GEAR" (x, y) "gray" ]
  in
  if model.armor == Nothing && model.weapon == Nothing then
    if List.length model.inventory > 0 then
      header
      ++ (List.indexedMap (inventoryItemView (x,y+2) action) model.inventory)
    else
      []
  else
    let
      equipment =
        equipmentView (x,y+2) action model

      hr =
        horizontalRule (x,y+2+equipCount)

      equipCount =
        List.length equipment

      items =
        List.map2 (inventoryItemView (x,y+3) action) [equipCount..9] model.inventory

    in
       header
       ++ equipment
       ++ hr
       ++ items

horizontalRule (x,y) =
  [ Graphics.render "" (x,y) "grey" ]

weaponView : Point -> Maybe Action -> Int -> Weapon -> Svg.Svg a
weaponView (x,y) action n weapon =
  let
    message =
      case action of
        Nothing ->
          "- "
          ++ Weapon.describe weapon
        Just action' ->
          "("
          ++ toString n
          ++ ") "
          ++ Action.describeWithDefault (Item.simple (Item.weapon weapon)) True action'
          ++ " "
          ++ Weapon.describe weapon

    color =
      if action == Just Action.drop then
        "red"
      else
        "lightgray"
  in
    Graphics.render message (x,y) color

armorView : Point -> Maybe Action -> Int -> Armor -> Svg.Svg a
armorView (x,y) action n armor =
  let
    desc =
      Armor.describe armor

    message =
      case action of
        Nothing ->
          "- "
          ++ desc
        Just action' ->
          "("
          ++ toString n
          ++ ") "
          ++ Action.describeWithDefault (Item.simple (Item.armor armor)) True action'
          ++ " "
          ++ desc

    color =
      if action == Just Action.drop then
        "red"
      else
        "lightgray"
  in
    Graphics.render message (x,y) color

equipmentView (x,y) action model =
  case model.weapon of
    Just weapon' ->
      case model.armor of
        Just armor' ->
          [ weaponView (x,y) action 0 weapon', armorView (x,y+1) action 1 armor' ]
        Nothing ->
          [ weaponView (x,y) action 0 weapon' ]

    Nothing ->
      case model.armor of
        Just armor' ->
          [ armorView (x,y) action 0 armor' ]
        Nothing ->
          [ ]

inventoryItemView: Point -> Maybe Action -> Int -> Item -> Svg.Svg a
inventoryItemView (x,y) action n item =
  let
    desc =
      case action of
        Nothing ->
          "- " ++ (Item.describe item)

        Just action' ->
          "("
          ++ (toString n)
          ++ ") "
          ++ Action.describeWithDefault item False action'
          ++ " "
          ++ Item.describe item

    color =
      case action of
        Nothing ->
          "lightgrey"

        Just act ->
          if act |> Action.canPerform item then
            if act == Action.drop then
              "red"
            else
              "lightgrey"
          else
            "darkgrey"
  in
    Graphics.render desc (x,y+n) color
