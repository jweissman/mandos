module Inventory exposing (view, itemAtIndex, size)

import Util
import Point exposing (Point)
import Warrior exposing (Model)
import Weapon exposing (Weapon)
import Armor exposing (Armor)
import Ring exposing (Ring)
import Helm exposing (Helm)
import Action exposing (Action)
import Item exposing (Item)
import Graphics
import Palette
import Language exposing (Language)

import Dict exposing (Dict)
import Svg

type Equipment = EquippedWeapon Weapon
               | EquippedArmor Armor
               | EquippedRing Ring
               | EquippedHelm Helm

-- HELPERS
size : Model -> Int
size model =
  model |> organize |> Dict.size

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

asItem : Equipment -> Item
asItem equipment =
  let toItem = case equipment of
    EquippedArmor armor -> Item.armor armor
    EquippedWeapon weapon -> Item.weapon weapon
    EquippedRing ring -> Item.ring ring
    EquippedHelm helm -> Item.helm helm
  in Item.simple toItem

equippedItems model =
  let
    weapon = case model.weapon of
      Nothing -> Nothing
      Just weapon' ->
        Just (EquippedWeapon weapon')

    armor = case model.armor of
      Nothing -> Nothing
      Just armor' ->
        Just (EquippedArmor armor')

    ring = case model.ring of
      Nothing -> Nothing
      Just ring' ->
        Just (EquippedRing ring')

    helm = case model.helm of
      Nothing -> Nothing
      Just helm' ->
        Just (EquippedHelm helm')

    equipment =
      [ weapon
      , armor
      , helm
      , ring
      ]
  in
    equipment
    |> List.filterMap identity
    |> List.map asItem

itemAtIndex : Int -> Model -> Maybe Item
itemAtIndex idx model =
  let
    equipment =
      equippedItems model

    gearCount =
      List.length equipment
  in
    if idx < List.length equipment then
      Util.getAt equipment idx
    else
      model |> organizedItemAt (idx - gearCount)

-- VIEW

view : Point -> Language -> Maybe Action -> Model -> List (Svg.Svg a)
view (x,y) lang action model =
  let
    act =
      not (action == Nothing)

    header =
      [ Graphics.render "GEAR" (x, y) Palette.secondaryLighter ]

    equipment =
      equipmentView (x,y+2) lang action model

    hr =
      horizontalRule (x,y+2+equipCount)

    equipCount =
      List.length equipment

    items =
      List.map2 (\n (ct,it) ->
        itemView (x,y+3) action n ct False it
      ) [equipCount..30] (model |> organize |> Dict.values)

  in
     header
     ++ equipment
     ++ hr
     ++ items

horizontalRule (x,y) =
  [ Graphics.render "---" (x,y) Palette.dim ]

equipmentView : Point -> Language -> Maybe Action -> Warrior.Model -> List (Svg.Svg a)
equipmentView (x,y) language action model =
  model
  |> equippedItems
  |> List.indexedMap (\n item -> itemView (x,y) action n 1 True item)

itemView: Point -> Maybe Action -> Int -> Int -> Bool -> Item -> Svg.Svg a
itemView (x,y) action idx count equipped item =
  let
    message =
      itemMessage idx action equipped item

    desc =
      if count > 1 then
        message ++ " (x" ++ (toString count) ++ ")"
      else
        message

    color =
      itemColor action equipped item

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
