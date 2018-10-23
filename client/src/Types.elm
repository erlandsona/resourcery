module Types exposing (Account(..), Deed(..), Description(..), Fellowship(..), Fibonacci(..), History(..), Idea(..), Incantation, Mastery(..), Name(..), Philosophy(..), Pipeline(..), Prowess(..), Quest(..), Reason(..), Scope(..), Skill(..), Sourcerer, Sprint(..), Summons(..), defSprint, estimate, fibonacci, period, sourcerersVelocity, speed)

import List.Extra as ListE
import Time exposing (Posix, utc)
import Time.Extra as TimE exposing (Interval(..))


type Summons
    = Summons Quest Fellowship


type Fellowship
    = Fellowship (List Sourcerer)


type Quest
    = Quest (List Incantation)



-- Not sure mixing billing and estimation here is ideal...


type Scope
    = Hours Int
    | End Posix
    | Monthly


type Sprint
    = Days Int


defSprint : Sprint
defSprint =
    Days 14


requiredHoursWorkedInASprintPeriod : Float
requiredHoursWorkedInASprintPeriod =
    80


speed : List Sourcerer -> Int
speed =
    List.foldl (sourcerersVelocity >> (+)) 0


period : List Sourcerer -> Float
period =
    toFloat << List.foldl (.history >> (\(Historical h) -> h) >> List.length >> (+)) 0


estimate : Summons -> Scope
estimate (Summons (Quest incantations) (Fellowship sourcerers)) =
    let
        fellowshipsAvgVelocity =
            (toFloat <| List.foldl (sourcerersVelocity >> (+)) 0 sourcerers)
                / (toFloat <| List.length sourcerers)

        -- hours
    in
    toFloat (List.foldl ((+) << fibonacci << .effort) 0 incantations)
        |> (\sum -> round <| (sum / fellowshipsAvgVelocity) * requiredHoursWorkedInASprintPeriod)
        |> Hours



-- Like Miles Per Hour... but instead we use Points per Sprint?


sourcerersVelocity : Sourcerer -> Int
sourcerersVelocity s =
    let
        (Historical deeds) =
            .history s

        twoWeeksInMillis =
            14 * 24 * 60 * 60 * 1000

        sprInt =
            defSprint
                |> (\(Days int) -> int)

        groupedDeeds =
            deeds
                |> List.sortBy (\(Deed _ timestamp) -> Time.posixToMillis timestamp)
                |> ListE.groupWhile
                    (\(Deed _ timeA) (Deed _ timeB) ->
                        TimE.diff Day utc timeA timeB <= sprInt
                    )

        sumOfDeeds =
            List.foldl ((\(Deed philosophy date) -> philosophy) >> fibonacci >> (+)) 0

        makeSprint ( a, aS ) =
            let
                lst =
                    a :: aS

                lng =
                    List.length lst
            in
            ( lng, sumOfDeeds lst * lng )

        groupedDeedsSummed =
            List.map makeSprint groupedDeeds

        avgSums =
            toFloat <| List.foldl (Tuple.second >> (+)) 0 groupedDeedsSummed

        sumOfLngs =
            toFloat <| List.foldl (Tuple.first >> (+)) 0 groupedDeedsSummed

        -- numOfSprints =
        --     toFloat <| List.length groupedDeeds
    in
    round <| avgSums / sumOfLngs



-- Once and Incantation is performed it is known as a Deed : Incantation -> Deed
-- Something is missing in the connection of History, Deeds, Period / Velocity etc...


type History
    = Historical (List Deed)


type Deed
    = Deed Philosophy Posix


type alias Incantation =
    -- Synonyms: Charm, Curse, Jinx
    { description : Idea
    , effort : Philosophy
    }


type Idea
    = AsA Account Description Reason


type Description
    = IWantTo String


type Reason
    = SoThat String


type Philosophy
    = GuessOf Fibonacci Mastery


type Fibonacci
    = One
    | Two
    | Three
    | Five
    | Eight
    | Thirteen


fibonacci : Philosophy -> Int
fibonacci (GuessOf complexity _) =
    case complexity of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Five ->
            5

        Eight ->
            8

        Thirteen ->
            13


type Pipeline
    = Pipeline (List Summons)



-- Prowess is too general there needs to be a distinction between:
-- the idea of the skill required to accomplish a task
-- and the accumulation of those skills defined by the understanding of a language or technology.


type Prowess
    = Prowess (List Skill)


type Mastery
    = MasterOf (List Skill)


type Skill
    = Skill String


type Name
    = Named String


type Account
    = Anonymous
    | Patron Name
    | Magic Sourcerer


type alias Sourcerer =
    { casting : Maybe Incantation -- head quest from summons
    , named : Name

    -- , summons : Summons -- head pipeline
    , journey : Pipeline -- tail pipeline
    , history : History -- List of Deeds / Incantations performed
    , skills : Prowess -- Seed of Skills + prowess attained from philosophies learned by completing Incantations on Quests...
    }
