module Estimate exposing (Scope(..), in_)

import Dict exposing (Dict)
import Fellow
import Helpers.Dict exposing (from)
import Helpers.List exposing (sum)
import ID exposing (ID)
import Incantation exposing (Philosophy(..), Quest)
import Journey exposing (Caesura(..))
import Prediction
import Result exposing (Result)
import Result.Extra as ResultE
import Set exposing (Set)
import Sourcerer
import Summons
import World exposing (World)



-- Like Miles Per Hour... but instead we use Points per Journey?


type Scope
    = Approximately Int Caesura


in_ : Caesura -> Summons.Id -> World -> Scope
in_ time str world =
    let
        { summons } =
            world

        result : Result String Summons.Info
        result =
            Summons.get str summons
    in
    ResultE.unpack
        (\_ -> Approximately 0 time)
        (withSummons time world)
        result


withSummons : Caesura -> World -> Summons.Info -> Scope
withSummons time { incantations, sourcerers } summons =
    let
        { quest, fellowship } =
            summons

        int =
            approximation fellowship quest sourcerers incantations
    in
    case time of
        Hours ->
            Approximately int Hours

        Days ->
            Approximately (roundDays int) Days

        Weeks ->
            Approximately (roundWeeks int) Weeks


roundDays : Int -> Int
roundDays int =
    round <| toFloat int / 8


roundWeeks : Int -> Int
roundWeeks int =
    round <| toFloat int / 40


cost : Incantation.Quest -> Incantation.Table -> Int
cost quest incantations =
    sum (.effort >> (\(GuessOf prediction _) -> prediction) >> Prediction.assess) <| (incantations |> from quest)


approximation :
    Fellow.Ship
    -> Quest
    -> Sourcerer.Table
    -> Incantation.Table
    -> Int -- Hours
approximation fellowship quest sourcerers incantations =
    round <| (*) (toFloat (cost quest incantations) / (toFloat <| sum (.tale >> Sourcerer.velocity) <| (sourcerers |> from fellowship))) (toFloat <| Journey.hours)
