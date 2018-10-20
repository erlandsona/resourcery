module Incantation exposing
    ( Id
    , Incantation
    , Philosophy
    , fibonacci
    , new
    )

import Incantation.Story exposing (Story)
import Prowess exposing (Prowess)


type Id
    = Id (Maybe Int)


type alias Incantation =
    -- Synonyms: Charm, Curse, Jinx
    { id : Id
    , description : Story
    , philosophy : Philosophy
    }


new : Story -> Philosophy -> Incantation
new =
    Incantation (Id Nothing)


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
