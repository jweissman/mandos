module Graph exposing (Graph, map, fold, minimumBy, match, node, edge, nodeWithEdges, tree, listNodes)
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

listNodes : Graph a -> List a
listNodes (Node n ns) =
  n :: (List.concatMap (listNodes) ns)

-- fold over edges
fold : ((a,a) -> b -> b) -> b -> Graph a -> b
fold f initial graph =
  List.foldr f initial (edges graph)

edges : Graph a -> List (a,a)
edges (Node n ns) =
  let
    edges' =
      List.map (\n' -> (n, nodeValue n')) ns
  in
    edges' ++ (List.concatMap edges ns)

edge : a -> a -> Graph a -> Graph a
edge n n' graph =
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
  minimumWhere f (\_ -> True) graph
  |> Maybe.withDefault (graph)

-- minimum by a comparator, filtered by a bool
minimumWhere : (a -> comparable) -> (a -> Bool) -> Graph a -> Maybe (Graph a)
minimumWhere f pred graph =
  let
    (Node n edges) =
      graph

    minEdges =
      edges
      |> List.filterMap (minimumWhere f pred)

    minRest =
      minEdges
      |> Util.minBy (\n' -> f (nodeValue n'))
  in
    case minRest of
      Just minRestNode ->
        let restMinDist = f (nodeValue minRestNode) in
        if (f n) < restMinDist && pred n then
          Just graph
        else
          minRest

      Nothing -> 
        if pred n then
          Just graph
        else
          Nothing

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

span : (a -> a -> comparable) -> List a -> Maybe (Graph a)
span f ls = tree f (\_ _ -> True) ls

tree : (a -> a -> comparable) -> (a -> a -> Bool) -> List a -> Maybe (Graph a)
tree f predicate ls =
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
            Just (tree' f predicate firstNode rest)

tree' : (a -> a -> comparable) -> (a -> a -> Bool) -> Graph a -> List a -> Graph a
tree' f pred graph ls =
  let
    weight = \x -> -- least f to a node in the graph
      graph
      |> minimumBy (f x)
      |> nodeValue
      |> f x 
      
    rest =
      ls |> List.sortBy weight

  in
    case (List.head rest) of
      Nothing ->
        graph

      Just elem ->
        let
          closestNode =
            graph 
            |> minimumWhere (f elem) (pred elem)

          graph' =
            case closestNode of
              Just closest ->
                graph |> edge (nodeValue closest) (elem)

              Nothing ->
                graph
        in
          case (List.tail rest) of
            Nothing ->
              graph'

            Just rest' ->
              rest' |> tree' f pred graph'
