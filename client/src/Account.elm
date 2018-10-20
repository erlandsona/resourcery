module Account exposing (Account, Id)

import Patron exposing (Patron)
import Sourcerer exposing (Sourcerer)


type Account
    = Patron
    | Sourcerer


type Id
    = Id (Maybe Int)
