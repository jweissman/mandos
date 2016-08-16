module Util -- exposing (detectEntity)

--import World
--import Entity

-- minimumBy impl from https://github.com/circuithub/elm-list-extra/blob/master/src/List/Extra.elm
minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f ls =
  let minBy x (y, fy) = let fx = f x in if fx < fy then (x, fx) else (y, fy)
  in case ls of
        [l']    -> Just l'
        l'::ls' -> Just <| fst <| List.foldl minBy (l', f l') ls'
        _       -> Nothing

