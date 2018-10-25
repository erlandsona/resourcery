module Types exposing
    ( Account(..)
    , Deed(..)
    , Description(..)
    , Fellowship(..)
    , Fibonacci(..)
    , History(..)
    , Idea(..)
    , Incantation
    , Journey(..)
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
    ,  estimate
       -- Private Functions used by estimate
       -- , costOf
       -- , defJourney
       -- , fibonacci
       -- , speed

    , velocity
    )

import List.Extra as ListE
import Skills exposing (Skills)
import Time exposing (Posix, utc)
import Time.Extra as TimE exposing (Interval(..))


type Summons
    = Summons Quest Fellowship


type Fellowship
    = Fellowship (List Sourcerer)


type Quest
    = Quest (List Incantation)



-- Not sure mixing billing and estimation here is ideal...


type
    Scope
    -- | End Posix
    -- | Monthly
    = Hours Int


type Journey
    = Days Int


defJourney : Journey
defJourney =
    Days 14


hoursWorkedPerTale : Float
hoursWorkedPerTale =
    80


estimate : Summons -> Scope
estimate (Summons (Quest incantations) (Fellowship f)) =
    costOf incantations
        |> (\(Hours cost) -> round <| (toFloat cost / toFloat (sum velocity f)) * hoursWorkedPerTale)
        |> Hours


costOf : List Incantation -> Scope
costOf =
    Hours << sum (.effort >> (\(GuessOf fib _) -> fib) >> fibonacci)



-- Like Miles Per Hour... but instead we use Points per Journey?


sprInt : Int
sprInt =
    defJourney
        |> (\(Days int) -> int)


sumOfDeeds : List Deed -> Int
sumOfDeeds =
    sum <| (\(Deed fib _) -> fib) >> fibonacci


sum : (a -> number) -> List a -> number
sum f =
    List.foldl (f >> (+)) 0


type alias Tale =
    { numOfDeeds : Float
    , sumOfPoints : Float
    }


organize : List Deed -> List ( Deed, List Deed )
organize =
    List.sortBy (\(Deed _ timestamp) -> Time.posixToMillis timestamp)
        >> ListE.groupWhile
            (\(Deed _ timeA) (Deed _ timeB) ->
                TimE.diff Day utc timeA timeB <= sprInt
            )


tale : ( Deed, List Deed ) -> Tale
tale ( deed, deeds ) =
    let
        lst =
            deed :: deeds

        numOfDeeds =
            List.length lst
    in
    { sumOfPoints = toFloat <| sumOfDeeds lst * numOfDeeds
    , numOfDeeds = toFloat numOfDeeds
    }


velocity : Sourcerer -> Int
velocity s =
    let
        (Historical deeds) =
            .history s

        tales =
            List.map tale (organize deeds)

        sumOfNumOfDeeds =
            sum .numOfDeeds tales

        sumOfAllPoints =
            sum .sumOfPoints tales

        -- numOfTales =
        --     toFloat <| List.length groupedDeeds
    in
    round (sumOfAllPoints / sumOfNumOfDeeds)



-- Once and Incantation is performed it is known as a Deed : Incantation -> Deed
-- Something is missing in the connection of History, Deeds, Period / Velocity etc...


type History
    = Historical (List Deed)


type Deed
    = Deed Fibonacci Posix


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


fibonacci : Fibonacci -> Int
fibonacci complexity =
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
    = Prowess Skills


type Mastery
    = MasterOf Skills


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
    , history : History -- List of Deeds / Incantations performed
    , skillSet : Prowess -- Seed of Skills + prowess attained from philosophies learned by completing Incantations on Quests...
    }


castSpell : Posix -> Sourcerer -> Incantation -> Sourcerer
castSpell timestamp s { effort } =
    let
        (GuessOf fib mastery) =
            effort
    in
    { s
        | history = Historical (.history s |> (\(Historical deeds) -> Deed fib timestamp :: deeds))
        , skillSet = improve (.skillSet s) mastery
    }


improve : Prowess -> Mastery -> Prowess
improve (Prowess skillSet) (MasterOf skills) =
    Prowess <| Skills.merge skillSet skills
