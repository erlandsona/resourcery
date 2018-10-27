module Types exposing
    ( Account(..)
    , Deed(..)
    , Description(..)
    , Fellowship(..)
    , Idea(..)
    , Incantation
    , Mastery(..)
    , Name(..)
    , Philosophy(..)
    , Pipeline(..)
    , Prowess(..)
    , Quest(..)
    , Reason(..)
    , Scope(..)
    , Sourcerer
    , Summons(..)
    , Tale(..)
    , Time(..)
    , castSpell
    ,  estimateIn
       -- Private Functions used by estimate
       -- , defJourney
       -- , assess
       -- , speed

    , velocity
    )

import List.Extra as ListE
import Magic exposing (Magic)
import Prediction exposing (..)
import Time exposing (Posix, utc)
import Time.Extra as TimE exposing (Interval(..))


type Summons
    = Summons Quest Fellowship


type Fellowship
    = Fellowship (List Sourcerer)


type Quest
    = Quest (List Incantation)


type Scope
    = In Time Int


type Time
    = Weeks
    | Days
    | Hours


defJourney : Time -> Int
defJourney time =
    case time of
        Hours ->
            80

        Days ->
            14

        Weeks ->
            2



-- Like Miles Per Hour... but instead we use Points per Journey?


sum : (a -> number) -> List a -> number
sum f =
    List.foldl (f >> (+)) 0


type Tale
    = Past (List Deed)



-- Non-Empty List of Deeds represented with a Tuple


velocity : Sourcerer -> Int
velocity s =
    let
        (Past deeds) =
            .history s

        deedInfo : ( Deed, List Deed ) -> ( Int, Int )
        deedInfo ( d, ds ) =
            let
                dds =
                    d :: ds

                countOfDeeds =
                    List.length dds
            in
            -- Weighting
            ( sum ((\(Deed fib _) -> fib) >> assess) dds * countOfDeeds
            , countOfDeeds
            )

        inscribed : List ( Int, Int )
        inscribed =
            List.sortBy (\(Deed _ timestamp) -> Time.posixToMillis timestamp) deeds
                |> ListE.groupWhile
                    (\(Deed _ timeA) (Deed _ timeB) ->
                        TimE.diff Day utc timeA timeB <= defJourney Days
                    )
                |> List.map deedInfo

        sumOfAssessments =
            toFloat <| sum Tuple.first inscribed

        sumOfCountsOfDeeds =
            toFloat <| sum Tuple.second inscribed
    in
    -- I need round to be the behavior of Integer division not truncate.
    round (sumOfAssessments / sumOfCountsOfDeeds)


estimateIn : Time -> Summons -> Scope
estimateIn time (Summons (Quest incantations) (Fellowship f)) =
    let
        cost =
            sum (.effort >> (\(GuessOf prediction _) -> prediction) >> assess) incantations

        hours =
            round <| (*) (toFloat cost / (toFloat <| sum velocity f)) (toFloat <| defJourney Hours)
    in
    case time of
        Hours ->
            In Hours hours

        Days ->
            In Days (round <| toFloat hours / 8)

        Weeks ->
            In Weeks (round <| toFloat hours / 40)


type Deed
    = Deed Prediction Posix


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
    = GuessOf Prediction Mastery


type Pipeline
    = Pipeline (List Summons)



-- Prowess is too general there needs to be a distinction between:
-- the idea of the skill required to accomplish a task
-- and the accumulation of those skills defined by the understanding of a language or technology.


type Prowess
    = Prowess Magic


type Mastery
    = MasterOf Magic


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
    , future : Pipeline -- tail pipeline
    , history : Tale -- List of Deeds / Incantations performed
    , prowess : Prowess -- Seed of Magic + prowess attained from philosophies learned by completing Incantations on Quests...
    }


castSpell : Posix -> Sourcerer -> Incantation -> Sourcerer
castSpell timestamp s { effort } =
    let
        (GuessOf fib mastery) =
            effort
    in
    { s
        | history = Past (.history s |> (\(Past deeds) -> Deed fib timestamp :: deeds))
        , prowess = improve (.prowess s) mastery
    }


improve : Prowess -> Mastery -> Prowess
improve (Prowess prowess) (MasterOf skills) =
    Prowess <| Magic.merge prowess skills
