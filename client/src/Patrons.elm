module Patrons exposing
    ( Patron
    ,  PatronId
       -- , new

    )


type alias Patron =
    { id : PatronId
    , name : String

    -- , treasury : Gold
    -- , timeline : Scope
    }


type PatronId
    = PatronId (Maybe Int)
