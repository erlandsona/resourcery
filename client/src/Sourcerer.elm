module Sourcerer exposing
    ( Sourcerer
    ,  SourcerersId
       -- , new

    )

import Objective



-- import Skill


type alias Sourcerer =
    { id : SourcerersId
    , name : String

    -- , skills : List Skill
    , objective : Objective
    , pipeline : List Objective
    }


type SourcerersId
    = SourcerersId (Maybe Int)



-- new : Name -> List Skill ->
