module Magic exposing
    ( Id
    , Info
    , Mastery(..)
    , Prowess(..)
    , Table
    , new
    )

import Dict exposing (Dict)
import ID exposing (ID)
import Set exposing (Set)


type alias Id =
    -- FIXME:
    -- type Id = Id ID
    ID


type alias Table =
    -- FIXME:
    -- type Magic
    --     = Magic Id Info
    Dict Id Info


type alias Info =
    { spell : String
    }


new : Info -> Table -> ( Id, Table )
new magic tbl =
    let
        { spell } =
            magic

        id =
            ID.new spell

        table =
            Dict.insert id magic tbl
    in
    ( id, table )



-- Sourcerers gain prowess from their Mastery of skills required to perform spells.


type Prowess
    = Skill (Set Id)


type Mastery
    = MasterOf (Set Id)
