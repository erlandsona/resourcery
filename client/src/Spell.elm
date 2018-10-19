module Spell exposing
    ( Incantation(..)
    , Spell
    , SpellId
    , new
    )

import Skill


type alias Spell =
    -- Synonyms: Charm, Curse, Jinx
    { id : SpellId
    , description : Story
    , philosophy : Fibonacci
    }


type SpellId
    = SpellId (Maybe Int)


type Incantation
    = Incantation (List Spell)


type Story
    = Story Account Idea Reason


new : Story -> Philosophy -> Spell
new =
    Spell Nothing


type Fibonacci
    = One Magic
    | Two Magic
    | Three Magic
    | Five Magic
    | Eight Magic
    | Thirteen Magic


fib : Fibonacci -> Int
fib f =
    case f of
        N1 ->
            1

        N2 ->
            2

        N3 ->
            3

        N5 ->
            5

        N8 ->
            8

        N13 ->
            13
