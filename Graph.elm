module Graph exposing (Graph(..), map, match, rankPaths, mergePaths, bfs)

type Graph a = Node a (List (Graph a))

map : (a -> b) -> Graph a -> Graph b
map f (Node x xs) =
  Node (f x) (List.map (map f) xs)

match : Graph a -> Graph a -> Bool
match node graph =
  let
    (Node _ rest) =
       graph
  in
    if node == graph then
      True
    else
      if List.length rest == 0 then
        False
      else
        List.any (match node) rest

-- merge paths together sorting by length
mergePaths : List (List a) -> List (List a) -> List (List a)
mergePaths x' y' =
  case (x',y') of
    (left,[]) ->
      left

    ([],right) ->
      right

    ((x :: xs), (y :: ys)) ->
      if List.length x < List.length y then
        x :: (mergePaths xs (y :: ys))
      else
        y :: (mergePaths (x :: xs) ys)

-- paths from focus to all other nodes...
rankPaths : Graph a -> List (List a)
rankPaths (Node label edges) =
  let
    paths =
      edges
      |> List.map rankPaths
      |> List.foldr mergePaths []
      |> List.map (List.append [label])
  in
    [[label]] ++ paths


bfs : (a -> Bool) -> Graph a -> Maybe (List a)
bfs predicate graph =
  graph
  |> rankPaths
  |> List.filter (\path -> 
    let
      last = (List.head (List.reverse path))
    in
      case last of
        Nothing -> False
        Just elem -> predicate elem 
    )
  |> List.head
