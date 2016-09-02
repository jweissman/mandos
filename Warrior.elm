module Warrior exposing (Model, init, step, takeDamage, enrich, collectsItem, drink, wield, wear, computeDamageAgainst, resistance, cardView)

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
  }

-- INIT

init : Point -> Model
init point =
  { hp = 20
  , maxHp = 20
  , direction = North
  , position = point
  , gold = 0
  , attack = 1
  , defense = 0
  , steps = 0
  , weapon = Nothing
  , armor = Nothing
  , inventory = []
  , timesGearChanged = 0
  }

power : Model -> Int
power model =
  case model.weapon of
    Nothing ->
      model.attack
    Just weapon ->
      model.attack + (Weapon.damage weapon)

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
  max 1 ((power model) - defense)

takeDamage : Int -> Model -> Model
takeDamage amount model =
  { model | hp = model.hp - amount }

enrich : Int -> Model -> Model
enrich amount model =
  { model | gold = model.gold + amount }

heal : Int -> Model -> Model
heal amount model =
  { model | hp = min model.maxHp (model.hp + amount) }

drink : Liquid -> Model -> Model
drink liquid model =
  case liquid of
    Liquid.Water -> 
      model |> heal 5

    --_ -> 
    --  model

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
      if model.weapon == Nothing then
         model |> wield weapon
      else
        model'

    Shield armor ->
      if model.armor == Nothing then
        model |> wear armor
      else
        model'

    _ ->
      model'

--losesItem : Item -> Model -> Model
--losesItem item model =


---

cardView : Point -> Maybe Action -> Model -> List (Svg.Svg a)
cardView (x,y) action model =
  let
    wielding =
      case model.weapon of
        Nothing -> "(none)"
        Just weapon ->
          Weapon.describe weapon

    wearing =
      case model.armor of
        Nothing -> "(none)"
        Just armor ->
          Armor.describe armor

    strength =
      toString (power model)

    resist =
      toString (resistance model)
  in
    [ Graphics.render "STATS" (x, y) "gray"
    , Graphics.render ("  STRENGTH: " ++ strength) (x, y+2) "lightgray"
    , Graphics.render ("RESISTANCE: " ++ resist) (x, y+3) "lightgray"
    ]
    ++ equipmentView (x,y+5) wielding wearing
     ++ inventoryView (x,y+10) action model.inventory

equipmentView (x,y) wielding wearing =
  [ Graphics.render "EQUIPMENT" (x, y) "gray"
  , Graphics.render ("WEAPON: " ++ wielding) (x, y+2) "lightgray"
  , Graphics.render (" ARMOR: " ++ wearing) (x, y+3) "lightgray"
  ]

inventoryView : Point -> Maybe Action -> List Item -> List (Svg.Svg a)
inventoryView (x,y) action items =
    [ Graphics.render "INVENTORY" (x, y) "gray"
    ] ++
    (List.indexedMap (inventoryItemView (x,y+2) action) items)

inventoryItemView: Point -> Maybe Action -> Int -> Item -> Svg.Svg a
inventoryItemView (x,y) action n item =
  let
    action' =
      action |> Maybe.withDefault (Action.defaultForItem item)

    desc =
      "(" ++ (toString n) ++ ") "
      ++ (Action.describe action')  ++ " "
      ++ (Item.describe item)

    color =
      case action of
        Nothing ->
          "lightgrey"

        Just act ->
          if act |> Action.canPerform item then
            "lightgrey"
          else
            "darkgrey"

  in
    Graphics.render desc (x,y+n) color
