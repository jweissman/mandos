module Ring exposing (Ring, describe, enchant, ruby, sapphire)

type Gemstone = Ruby
              | Sapphire
              | Turquoise
              | Diamond

type Ring = Gem Gemstone
          | Enchanted Int Ring

ruby : Ring
ruby =
  Gem Ruby

sapphire : Ring
sapphire =
  Gem Ruby

describe : Ring -> String
describe ring =
  case ring of
    Gem gem ->
      let gemName = case gem of
        Ruby -> "ruby"
        Sapphire -> "sapphire"
        Turquoise -> "turquoise"
        Diamond -> "diamond"
      in gemName ++ " ring"

    Enchanted n ring' ->
      "+" ++ (toString n) ++ " " ++ describe ring'


enchant : Ring -> Ring
enchant helm =
  case helm of
    Enchanted n helm' ->
      Enchanted (n+1) helm'

    _ ->
      Enchanted 1 helm
