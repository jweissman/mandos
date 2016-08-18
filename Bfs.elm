module Bfs exposing (bfs)

import Point exposing (Point, slide)
import Direction exposing (Direction)

import World exposing (Model)

import Set exposing (Set)
-- bfs impl
type alias Path = List Point

bfs : Point -> (Point -> Bool) -> Model -> Maybe Path
bfs source predicate model =
  bfs' [] [] source predicate (movesFrom model) 100 model

movesFrom : Model -> Point -> List (Point, Direction)
movesFrom model point =
  Direction.directions
  |> List.map (\direction -> (slide direction point, direction))
  |> List.filter (\(p,_) -> not (World.isBlocked p model))

bfs' : List (Point, Direction) -> List (Point, Direction) -> Point -> (Point -> Bool) -> (Point -> List (Point, Direction)) -> Int -> Model -> Maybe Path
bfs' visited frontier source predicate moves depth model =
  if depth < 0 then
    Nothing
  else
    let
      matches =
        \(v,_) -> predicate v

      maybeGoal =
        frontier
        |> List.filter matches
        |> List.head
    in
      case maybeGoal of
        Just (goal,_) ->
          let 
            path = 
              (constructPath (visited ++ frontier) source goal) 
          in
            Just (List.reverse path)

        Nothing ->
          if List.length frontier == 0 then
            let frontier' = moves source in
              bfs' visited frontier' source predicate moves (depth-1) model
          else
            let
              pointCode =
                (\({x,y},_) -> (x*10000) + y)

              visitedPositions =
                List.map fst newVisited

              newFrontier =
                frontier
                |> List.concatMap (\(pt,_) -> moves pt)
                |> List.filter (\(pt,_) -> not (List.member pt visitedPositions))
                |> uniqueBy pointCode

              newVisited =
                (visited ++ frontier)
            in
              if List.length frontier > 0 then
                --Debug.log ("bfs at depth: " ++ (toString (100-depth)))
                --Debug.log ("visited "++ (toString (List.length newVisited)) ++ ": " ++ (toString (List.map fst newVisited)))
                bfs' newVisited (newFrontier) source predicate moves (depth-1) model
              else
                Nothing

-- from list extras
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

constructPath : List (Point, Direction) -> Point -> Point -> Path
constructPath visited source destination =
  let
    maybeDestination =
      visited
      |> List.filter (\(pt,_) -> pt == destination)
      |> List.head
  in
     if source == destination then
        []
     else
       case maybeDestination of
         Nothing ->
           []

         Just (point, direction) ->
           let
             newDest =
               point
               |> slide (Direction.invert direction)
           in
             [destination] ++ (constructPath visited source newDest)


