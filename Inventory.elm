module Inventory exposing (view, itemAtIndex)

import Util
import Point exposing (Point)
import Warrior exposing (Model)
import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Action exposing (Action)
import Item exposing (Item)
import Graphics
import Palette

import Dict exposing (Dict)
import Svg

-- HELPERS

organize : Model -> Dict String (Int, Item)
organize model =
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

organizedItemAt : Int -> Model -> Maybe Item
organizedItemAt idx model =
  let
    inv = 
      model |> organize |> Dict.values

    maybeName = 
      Util.getAt inv idx 
  in
    case maybeName of
      Nothing ->
        Nothing

      Just (_,item) ->
        Just item

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
            model |> organizedItemAt (idx-2)

        Nothing ->
          if idx == 0 then
            Just (Item.simple (Item.weapon weapon'))
          else
            model |> organizedItemAt (idx-1)

    Nothing ->
      case model.armor of
        Just armor' ->
          if idx == 0 then
            Just (Item.simple (Item.armor armor'))
          else
            model |> organizedItemAt (idx-1)

        Nothing ->
          model |> organizedItemAt idx

-- VIEW

view : Point -> Maybe Action -> Model -> List (Svg.Svg a)
view (x,y) action model =
  let
    act = 
      not (action == Nothing)

    header =
      [ Graphics.render "GEAR" (x, y) Palette.secondaryLighter ]
  in
  if model.armor == Nothing && model.weapon == Nothing then
    if List.length model.inventory > 0 then
      header
      ++ (List.indexedMap (\n (ct,it) -> inventoryItemView (x,y+2) action n ct it) (model |> organize |> Dict.values))
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
        List.map2 (\n (ct,it) -> inventoryItemView (x,y+3) action n ct it) [equipCount..30] (model |> organize |> Dict.values)

    in
       header
       ++ equipment
       ++ hr
       ++ items

horizontalRule (x,y) =
  [ Graphics.render "---" (x,y) Palette.dim ]

weaponView : Point -> Maybe Action -> Int -> Weapon -> Svg.Svg a
weaponView (x,y) action n weapon =
  let
    asItem =
      (Item.simple (Item.weapon weapon))

    message =
      itemMessage n action True asItem

    color =
      itemColor action True asItem
      
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
      itemColor action True asItem
      
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
      itemColor action False item

  in
    Graphics.render desc (x,y+idx) color

itemColor action equipped item =
  case action of
    Nothing ->
      Palette.active

    Just act ->
      if act |> Action.canPerform equipped item then
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


--isEquipped : Item -> Model -> Bool
--isEquipped item player =
--  let
--    isArmor =
--      case player.armor of
--        Nothing ->
--          False
--        Just armor ->
--          item == (Item.simple (Item.armor armor))
--
--    isWeapon =
--      case player.weapon of
--        Nothing ->
--          False
--        Just weapon ->
--          item == (Item.simple (Item.weapon weapon))
--  in
--    isArmor
--    || isWeapon
