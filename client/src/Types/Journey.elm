module Journey exposing (Caesura(..), days, hours)

-- Caesura: noun. interruption
-- Interval conflicts with Time.Extra


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
