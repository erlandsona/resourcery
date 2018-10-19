module Account exposing
    ( Account(..)
    , AccountId
    , Patron
    , Philosopher
    , Sourcerer
    )

import Objective
import Skill


type Account
    = Patron
    | Sourcerer
    | Philosopher


type alias Patron =
    { id : AccountId
    , name : Name

    -- , treasury : Gold
    -- , timeline : Scope
    }


type alias Philosopher =
    { id : AccountId
    , name : Name
    }


type AccountId
    = AccountId (Maybe Int)


type alias Sourcerer =
    { id : AccountId
    , name : Name
    , objective : Objective
    , pipeline : List Objective
    , skills : List Skill
    }
