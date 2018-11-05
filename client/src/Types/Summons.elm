module Summons exposing
    ( Id
    , Info
    ,  Table
       -- , destroy

    , get
    , new
    )

import Dict exposing (Dict)
import Fellow
import Helpers.String exposing (concat)
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


new : Info -> Table -> ( Id, Table )
new info tbl =
    let
        key =
            id info

        table =
            Dict.insert key info tbl
    in
    ( key, table )


id : Info -> Id
id { fellowship, patronId, quest } =
    -- A reasonably uniquely identifiable string that will get hashed further to create the next id.
    patronId
        ++ concat quest
        ++ concat fellowship
        |> ID.new


get : Id -> Table -> Result String Info
get key table =
    let
        maybe =
            Dict.get key table
    in
    Result.fromMaybe
        ("Couldn't find Summons.Id: "
            ++ key
            ++ " in Summons.Table"
        )
        maybe
