module Types exposing
    ( Account(..)
    , Description(..)
    , Fibonacci(..)
    , Idea(..)
    , IdeaInfo
    , Incantation
    , Name(..)
    , Period(..)
    , Philosophy(..)
    , Pipeline(..)
    , Prowess(..)
    , Quest(..)
    , Reason(..)
    , Scope(..)
    , Skill(..)
    , Story(..)
    , Summons(..)
    , estimate
    , fibonacci
    )

import Date exposing (Date)


type Summons
    = Summons Quest Prowess


type Quest
    = Quest (List Incantation)


type Scope
    = Hours Int
    | End Date
    | Pay Period


type Period
    = Monthly
    | BiWeekly
    | Annual


estimate : Summons -> Scope
estimate (Summons (Quest incantations) _) =
    Hours <| List.sum <| List.map (fibonacci << .philosophy) incantations



-- velocity : Summons -> Scope
-- velocity (Summons (Quest incantations) _) =
--     fold (fibonnacci >> )


type alias Incantation =
    -- Synonyms: Charm, Curse, Jinx
    { description : Story
    , philosophy : Philosophy
    }


type Philosophy
    = GuessOf Fibonacci Prowess


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


type Idea
    = Idea IdeaInfo


type alias IdeaInfo =
    { description : Description
    , creator : Account
    }


type Pipeline
    = Pipeline (List Summons)


type Description
    = IWantTo String


type Prowess
    = Prowess (List Skill)


type Skill
    = Skill String


type Name
    = Name String


type Account
    = Patron Name
    | Sourcerer SourcerersInfo


type alias SourcerersInfo =
    { name : Name
    , casting : Maybe Incantation -- head quest from summons
    , summons : Summons -- head pipeline
    , journey : Pipeline -- tail pipeline
    , skills : Prowess -- Seed of Skills + prowess attained from philosophies learned by completing Incantations on Quests...
    }


type Story
    = AsA Account Description Reason


type Reason
    = SoThat String
