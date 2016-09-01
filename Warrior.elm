module Warrior exposing (Model, init, step, takeDamage, enrich, collectsItem, wield, wear, computeDamageAgainst, cardView)

import Direction exposing (Direction(..))
import Point exposing (Point, slide)

import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Item exposing (Item, ItemKind(..))

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
  { model | hp = min model.maxHp (model.hp + 1) }

wield : Weapon -> Model -> Model
wield weapon model =
  case model.weapon of
    Just weapon' ->
      { model | weapon = Just weapon
              , inventory = model.inventory ++ [Item.init (0,0) (Item.weapon weapon') 1]
      }

    Nothing ->
      { model | weapon = Just weapon }

wear : Armor -> Model -> Model
wear armor model =
  case model.armor of
    Just armor' ->
      { model | armor = Just armor
              , inventory = model.inventory ++ [Item.init (0,0) (Item.armor armor') 1]
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

---

cardView : Point -> Model -> List (Svg.Svg a)
cardView (x,y) model =
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
     ++ inventoryView (x,y+10) model.inventory

equipmentView (x,y) wielding wearing =
  [ Graphics.render "EQUIPMENT" (x, y) "gray"
  , Graphics.render ("WEAPON: " ++ wielding) (x, y+2) "lightgray"
  , Graphics.render (" ARMOR: " ++ wearing) (x, y+3) "lightgray"
  ]

inventoryView : Point -> List Item -> List (Svg.Svg a)
inventoryView (x,y) items =
    [ Graphics.render "INVENTORY" (x, y) "gray"
    ] ++
    (List.indexedMap (inventoryItemView (x,y+2)) items)

inventoryItemView: Point -> Int -> Item -> Svg.Svg a
inventoryItemView (x,y) n item =
  let desc = "(" ++ (toString n) ++ ") " ++ (Item.describe item) in
  Graphics.render desc (x,y+n) "lightgray"
