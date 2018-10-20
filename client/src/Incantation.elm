module Incantation exposing
    ( Incantation
    , Philosophy
    , fibonacci
    , new
    )

import Incantation.Story exposing (Story)
import Prowess exposing (Prowess)


type alias Incantation =
    -- Synonyms: Charm, Curse, Jinx
    { description : Story
    , philosophy : Philosophy
    }


new : Story -> Philosophy -> Incantation
new =
    Incantation


type Philosophy
    = Philosophy Fibonacci Prowess


type Fibonacci
    = One
    | Two
    | Three
    | Five
    | Eight
    | Thirteen


fibonacci : Philosophy -> Int
fibonacci (Philosophy number _) =
    case number of
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
