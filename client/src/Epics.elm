module Epics exposing
    ( Epic
    ,  EpicId
       -- , new

    )

import Objectives
import Patrons
import Sourcerers


type alias Epic =
    { id : EpicId
    , name : String
    , objectives : List Objective
    , patron : Patron
    , sourcerers : List Sourcerer
    }
