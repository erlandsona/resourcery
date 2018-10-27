module Prediction exposing (Prediction(..), assess)


type Prediction
    = One
    | Two
    | Three
    | Five
    | Eight
    | Thirteen


assess : Prediction -> Int
assess prediction =
    case prediction of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Five ->
            5

        Eight ->
            8

        Thirteen ->
            13
