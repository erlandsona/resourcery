module Incantation exposing
    ( Description(..)
    , Id
    , Idea(..)
    , Info
    , Philosophy(..)
    , Quest
    , Reason(..)
    , Table
    , id
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


type alias Quest =
    Set Id


id : Info -> Id
id { idea, effort } =
    let
        (AsA person (IWantTo description) (SoThat reason)) =
            idea

        accountId =
            case person of
                Generous patron ->
                    patron

                Magic sourcerer ->
                    sourcerer
    in
    ID.new accountId ++ description ++ reason
