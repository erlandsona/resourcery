module Patron exposing (Patron)

import Account.Name exposing (Name)


type Patron
    = Patron Info


type Id
    = Id (Maybe Int)


type alias Info =
    { id : Id
    , name : Name
    }
