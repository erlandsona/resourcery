module World exposing (World)

import Incantation
import Magic
import Patron
import Sourcerer
import Summons


type alias World =
    { incantations : Incantation.Table
    , magic : Magic.Table
    , patrons : Patron.Table
    , summons : Summons.Table
    , sourcerers : Sourcerer.Table
    }
