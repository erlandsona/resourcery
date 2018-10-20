module Incantation.Story exposing
    ( Reason
    ,  Story
       -- , new

    )

import Account.Name exposing (Name)
import Idea.Description exposing (Description)


type Story
    = Story Name Description Reason


type Reason
    = Reason String



-- new : Account -> Idea -> Reason -> Story
-- new a i r=
--     Story
