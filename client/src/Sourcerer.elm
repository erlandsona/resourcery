module Sourcerer exposing
    ( Id
    , Info
    , Table
    , improve
    ,  new
       -- , gen

    , velocity
    )

import Dict exposing (Dict)
import Helpers.List exposing (sum)
import History exposing (..)
import ID exposing (ID)
import Journey
import List.Extra as ListE
import Magic exposing (Mastery(..), Prowess(..))
import Name exposing (..)
import Prediction
import Random exposing (Generator)
import Set exposing (Set)
import Time exposing (utc)
import Time.Extra as TimE exposing (Interval(..))


type alias Id =
    -- FIXME:
    -- type Id = Id ID
    ID


type alias Table =
    -- FIXME:
    -- type Sourcerer
    --     = Sourcerer Id Info
    Dict Id Info


type alias Info =
    { name : Name
    , tale : Past -- List of Deeds / Incantations performed
    , skills : Prowess -- Seed of Magic + prowess attained from philosophies learned by completing Incantations on Quests...
    }


improve : Prowess -> Mastery -> Prowess
improve (Skill set) (MasterOf spells) =
    Skill (Set.union set spells)


velocity : Past -> Int
velocity tale =
    let
        deedInfo : ( Deed, Past ) -> ( Int, Int )
        deedInfo ( d, ds ) =
            let
                dds =
                    d :: ds

                countOfDeeds =
                    List.length dds
            in
            -- Weighting
            ( sum (History.prediction >> Prediction.assess) dds * countOfDeeds
            , countOfDeeds
            )

        inscribed : List ( Int, Int )
        inscribed =
            List.sortBy (\( _, timestamp ) -> Time.posixToMillis timestamp) tale
                |> ListE.groupWhile
                    (\( _, timeA ) ( _, timeB ) ->
                        TimE.diff Day utc timeA timeB <= Journey.days
                    )
                |> List.map deedInfo

        sumOfAssessments =
            toFloat <| sum Tuple.first inscribed

        sumOfCountsOfDeeds =
            toFloat <| sum Tuple.second inscribed
    in
    -- I need round to be the behavior of Integer division not truncate.
    round (sumOfAssessments / sumOfCountsOfDeeds)



-- WIP: gen : Info -> Generator Sourcerer
-- gen =
--     ID.gen |> Random.map Id


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
