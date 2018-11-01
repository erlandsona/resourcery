module Incantation exposing
    ( Description(..)
    , Id
    , Idea(..)
    , Info
    , Philosophy(..)
    , Quest
    , Reason(..)
    , Table
    , new
    )

import Account exposing (Person(..))
import Dict exposing (Dict)
import ID exposing (ID)
import Magic exposing (Mastery)
import Prediction exposing (Prediction)
import Set exposing (Set)


type alias Id =
    -- FIXME:
    -- type Id = Id ID
    ID


type alias Table =
    -- FIXME:
    -- type Incantation
    --     = Incantation Id Info
    Dict Id Info


type alias Info =
    -- Synonyms: Charm, Curse, Jinx
    { idea : Idea
    , effort : Philosophy
    }


type Idea
    = AsA Person Description Reason


type Description
    = IWantTo String


type Reason
    = SoThat String


type Philosophy
    = GuessOf Prediction Mastery


new : Info -> Table -> ( Id, Table )
new info tbl =
    let
        { idea, effort } =
            info

        (AsA person (IWantTo description) (SoThat reason)) =
            idea

        accountId =
            case person of
                Generous patron ->
                    patron

                Magic sourcerer ->
                    sourcerer

        id =
            ID.new accountId ++ description ++ reason

        table =
            Dict.insert id info tbl
    in
    ( id, table )


type alias Quest =
    Set Id
