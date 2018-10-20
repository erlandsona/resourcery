module Prowess exposing
    ( Prowess
    , new
    )

import Skill exposing (Skill)


type Prowess
    = Prowess (List Skill)


new : List Skill -> Prowess
new =
    Prowess
