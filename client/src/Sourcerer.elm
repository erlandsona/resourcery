module Sourcerer exposing (Sourcerer)

import Account.Name exposing (Name)
import Incantation exposing (Incantation)
import Prowess exposing (Prowess)
import Summons exposing (Summons)


type Sourcerer
    = Sourcerer Info


type Id
    = Id (Maybe Int)


type alias Info =
    { id : Id
    , name : Name
    , casting : Maybe Incantation -- head quest from summons
    , summons : Summons -- head pipeline
    , journey : Pipeline -- tail pipeline
    , skills : Prowess -- Seed of Skills + prowess attained from philosophies learned by completing Incantations on Quests...
    }


type Pipeline
    = Pipeline (List Summons)
