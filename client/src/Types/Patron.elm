module Patron exposing
    ( Id
    , Info
    , Table
    , id
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


id : Info -> Id
id { name } =
    let
        (Name str) =
            name
    in
    ID.new str
