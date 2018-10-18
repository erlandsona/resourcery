module Objectives exposing
    ( Objective
    ,  ObjectiveId
       -- , new

    )

import Patrons


type Category
    = Challenge
    | Journey
    | Quest


type Fibonacci
    = N1
    | N2
    | N3
    | N5
    | N8
    | N13


type alias Objective =
    { id : ObjectiveId
    , name : String
    , patron : Patron
    , kind : Category
    , philosophy : Fibonacci
    }


type ObjectiveId
    = ObjectiveId (Maybe Int)
