module Fellow exposing
    ( Id
    , Info
    , Ship
    , Table
    , new
    )

import Dict exposing (Dict)
import ID exposing (ID)
import Incantation
import Set exposing (Set)
import Sourcerer


type alias Id =
    -- FIXME:
    -- type Id = Id ID
    ID


type alias
    Table
    -- FIXME:
    -- type Fellow
    --     = Fellow Id Info
    =
    Dict Id Info


type alias Info =
    { sourcererId : Sourcerer.Id
    , casting : Spell
    }


type Spell
    = Maybe Incantation.Id -- Nothing, Just (IncantationId <| Id 1)
    | Blocked -- , Because "Waiting on Client?"


type Blocked
    = Because String


new : Info -> Table -> ( Id, Table )
new info tbl =
    let
        { sourcererId } =
            info

        id =
            ID.new sourcererId

        table =
            Dict.insert id info tbl
    in
    ( id, table )


type alias Ship =
    Set Id
