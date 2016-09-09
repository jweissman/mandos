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
import Palette

import Graphics
import Svg


import Dict exposing (Dict)

-- MODEL

type alias Model =
  { hp : Int
  , maxHp : Int
  , direction : Direction
  , position : Point
  , gold : Int
  , strength : Int
  , steps : Int
  , weapon : Maybe Weapon
  , armor : Maybe Armor
  -- todo , ring : Maybe Ring
  -- todo , helm : Maybe Helm
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
  , strength = 100
  --,  = 0
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
      model.strength
    Just weapon ->
      model.strength + (Weapon.averageDamage weapon)

resistance : Model -> Int
resistance model =
  case model.armor of
    Nothing ->
      model.strength
    Just armor ->
      model.strength + (Armor.absorption armor)

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
        model.strength + Weapon.damage model.steps model.timesGearChanged weapon
      Nothing ->
        model.strength
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

    Potion effect ->
      case effect of
        Liquid.GainLife ->
          model
          |> gainHp 2

gainHp : Int -> Model -> Model
gainHp n model =
  let hp' = model.maxHp + n in
  { model | hp = hp'
          , maxHp = hp'
        }

sortInventory : Model -> Model
sortInventory model =
  let inventory' = model.inventory |> List.sortBy (\item -> Item.describe item) in
  { model | inventory = inventory' }

addToInventory item model = 
  let 
    model' = 
      { model | inventory = model.inventory ++ [item] }
  in
    model' |> sortInventory

wield : Weapon -> Model -> Model
wield weapon model =
  case model.weapon of
    Just weapon' ->
      let oldWeapon = Item.init (0,0) (Item.weapon weapon') (1000000 + model.timesGearChanged) in
      { model | weapon = Just weapon
              --, inventory = model.inventory ++ []
              , timesGearChanged = model.timesGearChanged + 1
      }
      |> addToInventory oldWeapon

    Nothing ->
      { model | weapon = Just weapon }


sheatheWeapon : Model -> Model
sheatheWeapon model =
  case model.weapon of
    Nothing ->
      model

    Just weapon ->
      let oldWeapon = Item.init (0,0) (Item.weapon weapon) (1000000 + model.timesGearChanged) in
      { model | weapon = Nothing
              , timesGearChanged = model.timesGearChanged + 1
            }
      |> addToInventory oldWeapon


takeOffArmor : Model -> Model
takeOffArmor model =
  case model.armor of
    Nothing ->
      model

    Just armor ->
      let oldArmor = Item.init (0,0) (Item.armor armor) (1000000 + model.timesGearChanged) in
      { model | armor = Nothing
              , timesGearChanged = model.timesGearChanged + 1
            }
            |> addToInventory oldArmor

wear : Armor -> Model -> Model
wear armor model =
  case model.armor of
    Just armor' ->
      let oldArmor = Item.init (0,0) (Item.armor armor') (1000000 + model.timesGearChanged) in
      { model | armor = Just armor
              --, inventory = model.inventory ++ []
              , timesGearChanged = model.timesGearChanged + 1
      }
      |> addToInventory oldArmor

    Nothing ->
      { model | armor = Just armor }

collectsItem : Item -> Model -> Model
collectsItem item model =
  let
    model' =
      model |> addToInventory item
      --{ model | inventory = model.inventory ++ [item] }
      --        |> sortInventory
  in 
    case item.kind of
      Arm weapon ->
        case model.weapon of
          Nothing ->
            model |> wield weapon
          Just weapon' ->
            model'

      Shield armor ->
        case model.armor of
          Nothing ->
            model |> wear armor
          Just armor' ->
            model'

      _ ->
        model'

organizedInventory : Model -> Dict String (Int, Item) --List (Int, Item)
organizedInventory model =
  let 
    countItems = \it dict -> 
      dict 
      |> Dict.update (Item.describe it) (\entry -> 
        case entry of 
          Nothing -> 
            Just (1,it)
          Just (ct',it') ->
            Just (ct' + 1, it')
      )
  in
    model.inventory
    |> List.foldr countItems Dict.empty

organizedInventoryItemAt : Int -> Model -> Maybe Item
organizedInventoryItemAt idx model =
  let
    inv = 
      model 
      |> organizedInventory
      |> Dict.values

    maybeName = 
      Util.getAt inv idx 
  in
    case maybeName of
      Nothing ->
        Nothing

      Just (_,item) ->
        Just item
        --model.inventory
        --|> List.filter (\it -> (Item.describe it) == name)
        --|> List.head

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
            model |> organizedInventoryItemAt (idx-2)
            --Util.getAt model.inventory (idx-2)

        Nothing ->
          if idx == 0 then
            Just (Item.simple (Item.weapon weapon'))
          else
            model |> organizedInventoryItemAt (idx-1)
            --Util.getAt model.inventory (idx-1)

    Nothing ->
      case model.armor of
        Just armor' ->
          if idx == 0 then
            Just (Item.simple (Item.armor armor'))
          else
            model |> organizedInventoryItemAt (idx-1)
            --Util.getAt model.inventory (idx-1)

        Nothing ->
          model |> organizedInventoryItemAt idx
          --Util.getAt model.inventory idx
  

-- VIEW
cardView : Point -> Maybe Action -> Model -> List (Svg.Svg a)
cardView (x,y) action model =
  let
    strength =
      toString (power model)

    resist =
      toString (resistance model)

    stats =
      [ Graphics.render "STATS" (x, y) Palette.dim
      , Graphics.render ("  STRENGTH: " ++ strength) (x, y+2) Palette.inactive
      , Graphics.render ("RESISTANCE: " ++ resist) (x, y+3) Palette.inactive
      ]

    inventory =
      inventoryView (x,y+6) action model
  in
    stats
    ++ inventory

inventoryView : Point -> Maybe Action -> Model -> List (Svg.Svg a)
inventoryView (x,y) action model =
  let
    act = 
      not (action == Nothing)

    header =
      [ Graphics.render "GEAR" (x, y) (if act then Palette.bright else Palette.dim) ] --"white" ]
  in
  if model.armor == Nothing && model.weapon == Nothing then
    if List.length model.inventory > 0 then
      header
      ++ (List.indexedMap (\n (ct,it) -> inventoryItemView (x,y+2) action n ct it) (model |> organizedInventory |> Dict.values))
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
        List.map2 (\n (ct,it) -> inventoryItemView (x,y+3) action n ct it) [equipCount..30] (model |> organizedInventory |> Dict.values)
        --List.map2 (\n (ct,it) -> inventoryItemView (x,y+3) action n ct it) [equipCount..30] model.inventory

    in
       header
       ++ equipment
       ++ hr
       ++ items

horizontalRule (x,y) =
  [ Graphics.render "---" (x,y) Palette.inactive ]

weaponView : Point -> Maybe Action -> Int -> Weapon -> Svg.Svg a
weaponView (x,y) action n weapon =
  let
    asItem =
      (Item.simple (Item.weapon weapon))

    message =
      itemMessage n action True asItem

    color =
      itemColor action asItem
      
  in
    Graphics.render message (x,y) color

armorView : Point -> Maybe Action -> Int -> Armor -> Svg.Svg a
armorView (x,y) action n armor =
  let
    asItem =
      (Item.simple (Item.armor armor))

    message =
      itemMessage n action True asItem

    color =
      itemColor action asItem
      
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

inventoryItemView: Point -> Maybe Action -> Int -> Int -> Item -> Svg.Svg a
inventoryItemView (x,y) action idx count item =
  let
    message = 
      itemMessage idx action False item 

    desc =
      if count > 1 then
        message ++ " (x" ++ (toString count) ++ ")"
      else
        message

    color =
      itemColor action item
      
  in
    Graphics.render desc (x,y+idx) color

itemColor action item =
  case action of
    Nothing ->
      Palette.default

    Just act ->
      if act |> Action.canPerform item then
        if act == Action.drop then
          Palette.warning
        else
          Palette.active
      else
        Palette.inactive

itemMessage n action equipped item =
  case action of
    Nothing ->
      "- "
      ++ Item.describe item
    Just action' ->
      "("
      ++ Util.toAlpha n
      ++ ") "
      ++ Action.describeWithDefault item equipped action'
      ++ " "
      ++ Item.describe item
