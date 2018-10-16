module Journey exposing
    ( Journey
    ,  JourneyId
       -- , new

    )

import Patron



-- Related to Jira: Feature?


type alias Journey =
    { id : JourneyId
    , name : String
    , patron : Patron
    }


type JourneyId
    = JourneyId (Maybe Int)
