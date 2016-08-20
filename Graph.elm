module Graph exposing (Graph, map, minimumBy, match, node, edge, nodeWithEdges, tree)
import Util

type Graph a = Node a (List (Graph a))

node x =
  Node x []

nodeWithEdges x edges =
  Node x edges

-- map values
map : (a -> b) -> Graph a -> Graph b
map f (Node n ns) =
  Node (f n) (List.map (map f) ns)

-- map nodes themselves (i.e., wrapped, with structure)
mapNodes : (Graph a -> Graph a) -> Graph a -> Graph a
mapNodes f (Node n ns) =
  f (Node n (List.map (mapNodes f) ns))

edge : a -> a -> Graph a -> Graph a
edge n n' graph =
  --Debug.log ("Edge from " ++ (toString n) ++ " -> " ++ (toString n'))
  mapNodes (joinMatching n n') graph

joinMatching : a -> a -> Graph a -> Graph a
joinMatching n node' trialNode =
  let (Node n' _) = trialNode in
  if n' == n then
    connect' trialNode (node node')
  else
    trialNode

connect' : Graph a -> Graph a -> Graph a
connect' (Node n ns) n' = 
  Node n (n' :: ns)

nodeValue : Graph a -> a
nodeValue (Node n _) = n

minimumBy : (a -> comparable) -> Graph a -> Graph a
minimumBy f graph =
  let
    (Node n edges) =
      graph

    restMinDist =
      f (nodeValue minRest)

    minEdges =
      edges
      |> List.map (minimumBy f)

    minRest =
      minEdges
      |> Util.minBy (\n' -> f (nodeValue n'))
      |> Maybe.withDefault graph
  in
   if (f n) < restMinDist  then
     graph
   else
     minRest

match : Graph a -> Graph a -> Bool
match n graph =
  let
    (Node _ rest) =
       graph
  in
    if n == graph then
      True
    else
      if List.length rest == 0 then
        False
      else
        List.any (match n) rest

--undirected : List a -> Maybe (Graph a)
--undirected (x :: xs) =
--  let
--    undirectEdge = \x' -> 
--      Node x' [
--        (undirected (xs |> List.filter (\x'' -> not (x'' == x'))))
--        --|> List.filterMap identity --(Maybe.withDefault False)
--      ]
--
--    edges =
--      List.map undirectEdge xs
--  in
--    Node x edges
  --case (List.head ls, List.tail ls) of
  --  (Nothing,Nothing) -> Nothing
  --  (Just x,Nothing) -> node x
  --  (Just x, Just xs) ->
  -- form an edge to every other element in list


-- given a function f used to weigh 'distances' between elements of a list form a graph 
-- (not necessarily minimum spanning tree but we're maybe close?)
-- maybe we should just directly implement prim or kruskal here!
tree : (a -> a -> comparable) -> List a -> Maybe (Graph a)
tree f ls =
  case (List.head ls) of
    Nothing ->
      Nothing

    Just elem ->
      let
        firstNode =
          node elem
      in
        case (List.tail ls) of
          Nothing ->
            Just firstNode

          Just rest ->
            Just (tree' f firstNode rest)

tree' : (a -> a -> comparable) -> Graph a -> List a -> Graph a
tree' f graph ls =
  let
    dist = \x -> -- least f to a node in the graph
      graph
      |> minimumBy (f x)
      |> nodeValue
      |> f x 
      
    rest =
      ls |> List.sortBy dist -- |> List.reverse

  in
    case (List.head rest) of
      Nothing ->
        graph

      Just elem ->
        let
          closestNode =
            graph |> minimumBy (f elem)

          graph' =
            graph |> edge (nodeValue closestNode) (elem)
        in
          case (List.tail rest) of
            Nothing ->

              Debug.log ("Closest node to " ++ (toString elem) ++ ": "++ (toString closestNode))
              Debug.log ("graph before connection: " ++ toString graph)
              Debug.log ("graph after connection: " ++ toString graph')
              --Debug.log (toString closestNode)
              graph'

            Just rest' ->
              Debug.log ("Closest node to " ++ (toString elem) ++ ": "++ (toString closestNode))
              Debug.log ("graph before connection: " ++ toString graph)
              Debug.log ("graph after connection: " ++ toString graph')
              rest' |> tree' f graph'
