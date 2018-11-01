module Helpers.Dict exposing (from)

import Dict exposing (Dict)
import Set exposing (Set)


from : Set comparable -> Dict comparable v -> List v
from set =
    let
        member : comparable -> v -> Bool
        member id _ =
            Set.member id set
    in
    Dict.filter member >> Dict.values
