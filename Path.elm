module Path exposing (Path, find, findBy)

import Point exposing (Point, slide)
import Direction exposing (Direction)

import Util

-- bfs impl
type alias Path = List Point

find : Point -> Point -> (Point -> List (Point, Direction)) -> Maybe Path
find dst src moves =
  findBy (\pt -> pt == dst) moves src

findBy : (Point -> Bool) -> (Point -> List (Point, Direction)) -> Point -> Maybe Path
findBy predicate moves source =
  findBy' [] [] source predicate moves 100

findBy' : List (Point, Direction) -> List (Point, Direction) -> Point -> (Point -> Bool) -> (Point -> List (Point, Direction)) -> Int -> Maybe Path
findBy' visited frontier source predicate moves depth =
  if depth < 0 then
    Nothing
  else
    let
      maybeGoal =
        frontier
        |> List.filter (predicate << fst)
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
              findBy' visited frontier' source predicate moves (depth-1)
          else
            let
              visitedPositions =
                List.map fst newVisited

              newFrontier =
                frontier
                |> List.concatMap (moves << fst)
                |> List.filter (\(pt,_) -> not (List.member pt visitedPositions))
                |> Util.uniqueBy (Point.code << fst)

              newVisited =
                (visited ++ frontier)
            in
              if List.length frontier > 0 then
                findBy' newVisited (newFrontier) source predicate moves (depth-1)
              else
                Nothing

constructPath : List (Point, Direction) -> Point -> Point -> Path
constructPath visited source destination =
  let
    isDestination = \pt ->
      pt == destination

    maybeDestination =
      visited
      |> List.filter (isDestination << fst)
      |> List.head
  in
     if isDestination source then
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


---

