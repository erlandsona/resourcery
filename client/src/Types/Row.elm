module Row exposing (new)

import Dict exposing (Dict)
import ID exposing (ID)


new : (value -> comparable) -> value -> ( comparable, Dict comparable value )
new key value =
    let
        id =
            key value

        table =
            Dict.fromList [ ( id, value ) ]
    in
    ( id, table )
