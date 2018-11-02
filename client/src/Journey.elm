module Journey exposing (Caesura(..), Scope(..), days, hours)

-- Caesura: noun. interruption
-- Interval conflicts with Time.Extra


type Scope
    = In Caesura Int


type Caesura
    = Weeks
    | Days
    | Hours


hours : Int
hours =
    80


days : Int
days =
    14
