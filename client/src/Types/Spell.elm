module Spell exposing (cast)

import Incantation exposing (Philosophy(..))
import Sourcerer
import Time exposing (Posix)


cast : Posix -> Sourcerer.Info -> Incantation.Info -> Sourcerer.Info
cast timestamp sourcerer { effort } =
    let
        (GuessOf fib mastery) =
            effort

        { skills, tale } =
            sourcerer
    in
    { sourcerer
        | tale = ( fib, timestamp ) :: tale
        , skills = Sourcerer.improve skills mastery
    }
