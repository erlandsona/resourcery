module Account exposing (Person(..))

import Patron
import Sourcerer


type Person
    = Generous Patron.Id
    | Magic Sourcerer.Id
