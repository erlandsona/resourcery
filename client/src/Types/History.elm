module History exposing
    ( Deed
    , Past
    , prediction
    , timestamp
    )

import ID exposing (ID)
import Prediction exposing (Prediction)
import Time exposing (Posix)


type alias Past =
    List Deed


type alias Deed =
    ( Prediction, Posix )


prediction : Deed -> Prediction
prediction =
    Tuple.first


timestamp : Deed -> Posix
timestamp =
    Tuple.second
