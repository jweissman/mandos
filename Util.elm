module Util exposing (minBy, uniqueBy, getAt, dropWhile, everyNth, mapEveryNth, sample, zip, filterChamp, toAlpha, fromAlpha)

import Point exposing (Point)
import Direction exposing (Direction(..))
import Set exposing (Set)
---import Mouse

import String


alphabet =
  ['a','b','c','e','f','g','h','j','k','l','m','n','o','p','q','r','s','t','u','v','x','y','z','-']

toAlpha : Int -> String
toAlpha idx =
  let alpha = (getAt alphabet idx |> Maybe.withDefault '-') in
  String.fromChar alpha

fromAlpha : Char -> Int
fromAlpha ch =
  elemIndex ch alphabet
  |> Maybe.withDefault -1

-- deterministically 'sample' a list based on two variables
sample : Int -> Int -> a -> List a -> a
sample m n zero ls =
  getAt ls ((m ^ 31 + n) % (max 1 (List.length ls - 1)))
  |> Maybe.withDefault zero

filterChamp : List a -> List a
filterChamp ls =
  let champ' = champ 100 in
  ls
  |> List.indexedMap (\n a -> (n,a)) 
  |> List.filter (\(n,_) -> 
    getAt champ' n
    |> Maybe.withDefault False)
  |> List.map snd

-- binary champernowne
champernowne : Int -> Bool
champernowne n =
  getAt (champ n) n
  |> Maybe.withDefault False

champ : Int -> List Bool
champ n =
  List.concatMap toBools [1..n]

toBools : Int -> List Bool
toBools n =
  let 
    lsb = 
      if n % 2 == 0 then 
        [False] 
      else 
        [True] 
  in
    if n < 2 then
      lsb
    else
      (toBools (n//2)) ++ lsb

-- 
--pick

everyNth n ls =
  case (ls |> List.drop (n-1)) of
    [] ->
      []
    (head :: rest) ->
      head :: (everyNth n rest)

mapEveryNth n f ls =
  let ls' = List.take (n-1) ls in
  case (List.drop (n-1) ls) of
    [] ->
      ls'
    (head :: rest) ->
      ls' ++ ((f head) :: (mapEveryNth n f rest))


zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) ->
        (x,y) :: zip xs' ys'

    (_, _) ->
        []

-- helpers from list extras
minBy : (a -> comparable) -> List a -> Maybe a
minBy f ls =
  let minBy x (y, fy) = let fx = f x in if fx < fy then (x, fx) else (y, fy)
  in case ls of
        [l']    -> Just l'
        l'::ls' -> Just <| fst <| List.foldl minBy (l', f l') ls'
        _       -> Nothing

uniqueBy f list =
  uniqueHelp f Set.empty list

uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a
uniqueHelp f existing remaining =
  case remaining of
    [] ->
      []

    first :: rest ->
      let computedFirst = f first in
      if Set.member computedFirst existing then
        uniqueHelp f existing rest
      else
        first :: uniqueHelp f (Set.insert computedFirst existing) rest

getAt : List a -> Int -> Maybe a
getAt xs idx = List.head <| List.drop idx xs

dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then dropWhile predicate xs
               else list

elemIndex : a -> List a -> Maybe Int
elemIndex x = findIndex ((==)x)

findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex p = List.head << findIndices p

findIndices : (a -> Bool) -> List a -> List Int
findIndices p = List.map fst << List.filter (\(i,x) -> p x) << List.indexedMap (,)
