module Challenge exposing
    ( Challenge
    ,  ChallengeId
       -- , new

    )

import Patron



-- Related to Jira: Bug?


type alias Challenge =
    { id : ChallengeId
    , name : String
    , patron : Patron
    }


type ChallengeId
    = ChallengeId (Maybe Int)
