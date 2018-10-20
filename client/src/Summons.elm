module Summons exposing
    ( Summons
    , new
    )

import Date exposing (Date)
import Prowess exposing (Prowess)
import Quest exposing (Quest)


type Summons
    = Summons Quest Prowess


new : Quest -> Prowess -> Summons
new =
    Summons


type Scope
    = Hours Int
    | End Date
    | Pay Period


type Period
    = Monthly
    | BiWeekly
    | Annual



-- estimate : Summons -> Scope
-- estimate (Summons (Quest incantations) _) =
--     sum (Incantation.fibonacci << .philosophy) incantations
-- velocity : Summons -> Scope
-- velocity (Summons (Quest incantations) _) =
--     fold (fibonnacci >> )
