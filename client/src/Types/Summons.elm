module Summons exposing
    ( Id
    , Info
    , Table
    , get
    , id
    )

import Dict exposing (Dict)
import Fellow
import ID exposing (ID)
import Incantation exposing (Quest)
import Patron
import Result exposing (Result)
import Set exposing (Set)
import Sourcerer


type alias Id =
    -- FIXME:
    -- type Id = Id ID
    ID


type alias Table =
    -- FIXME:
    -- type Summons
    --     = Summons Id Info
    Dict Id Info


type alias Info =
    { quest : Quest
    , fellowship : Fellow.Ship
    , patronId : Patron.Id
    }


id : Info -> Id
id { fellowship, patronId, quest } =
    let
        key =
            -- A reasonably uniquely identifiable
            -- string that will get hashed further
            -- to create the next id.
            patronId
                ++ Set.foldl (++) "" quest
                ++ Set.foldl (++) "" fellowship
    in
    ID.new key


get : Id -> Table -> Result String Info
get key =
    Result.fromMaybe
        ("Couldn't find Summons.Id: "
            ++ key
            ++ " in Summons.Table"
        )
        << Dict.get key
