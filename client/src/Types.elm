module Types exposing (Account(..), Deed(..), Description(..), Fellowship(..), Fibonacci(..), History(..), Idea(..), Incantation, Mastery(..), Name(..), Period(..), Philosophy(..), Pipeline(..), Prowess(..), Quest(..), Reason(..), Scope(..), Skill(..), Sourcerer, Summons(..), estimate, fibonacci, period, velocity)

import Date exposing (Date)


type Summons
    = Summons Quest Fellowship


type Fellowship
    = Fellowship (List Sourcerer)


type Quest
    = Quest (List Incantation)


type Scope
    = Hours Int
    | End Date
    | Pay Period


type Period
    = Sprint -- Two weeks
    | Month -- ~ Four weeks


period : Period -> Float
period p =
    let
        daysInAWeek =
            7

        daysInAYear =
            365

        monthsInAYear =
            12

        daysInAMonth =
            daysInAYear / monthsInAYear

        weeksInAMonth =
            (daysInAYear / monthsInAYear) / daysInAWeek

        hoursPerWeekWorked =
            40
    in
    case p of
        Sprint ->
            80

        -- hours given a 40 hour week
        Month ->
            hoursPerWeekWorked * weeksInAMonth


estimate : Period -> Summons -> Scope
estimate per (Summons (Quest incantations) (Fellowship sourcerers)) =
    let
        history =
            History <| List.concatMap (.history >> (\(History deeds) -> deeds)) sourcerers
    in
    List.foldl ((+) << fibonacci << .effort) 0 incantations
        -- Need to conretize this idea.
        |> (\sum -> round (sum / velocity history per))
        |> Hours



-- Like Miles Per Hour... but instead we use Points per Period


velocity : History -> Period -> Float
velocity (History deeds) per =
    List.foldl ((\(Deed philosophy) -> philosophy) >> fibonacci >> (+)) 0 deeds
        |> (\sum -> sum / period per)



-- Once and Incantation is performed it is known as a Deed : Incantation -> Deed
-- Something is missing in the connection of History, Deeds, Period / Velocity etc...


type History
    = History (List Deed)


type Deed
    = Deed Philosophy


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


fibonacci : Philosophy -> Float
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
    = Mastery (List Skill)


type Skill
    = Skill String


type Name
    = Name String


type Account
    = Anonymous
    | Patron Name
    | Magic Sourcerer


type alias Sourcerer =
    { name : Name
    , casting : Maybe Incantation -- head quest from summons
    , summons : Summons -- head pipeline
    , journey : Pipeline -- tail pipeline
    , history : History -- List of Deeds / Incantations performed
    , skills : Prowess -- Seed of Skills + prowess attained from philosophies learned by completing Incantations on Quests...
    }
