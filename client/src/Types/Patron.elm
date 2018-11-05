module Patron exposing
    ( Id
    , Info
    , Table
    , new
    )

import Dict exposing (Dict)
import ID exposing (ID)
import Name exposing (Name(..))



-- Should be type Id = Id ID
-- once we get comparables for any type
-- this can be updated.


type alias Id =
    -- FIXME:
    -- type Id = Id ID
    ID


type alias Table =
    -- FIXME:
    -- type Patron
    --     = Patron Id Info
    Dict Id Info


type alias Info =
    { name : Name
    }


new : Info -> Table -> ( Id, Table )
new info tbl =
    let
        { name } =
            info

        (Name str) =
            name

        id =
            ID.new str

        table =
            Dict.insert id info tbl
    in
    ( id, table )
