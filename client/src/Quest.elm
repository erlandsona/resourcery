module Quest exposing
    ( Quest
    ,  QuestId
       -- , new

    )

import Patron



-- Related to Jira: Epic?


type alias Quest =
    { id : QuestId
    , name : String
    , patron : Patron
    }


type QuestId
    = QuestId (Maybe Int)
