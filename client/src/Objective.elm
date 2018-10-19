module Objective exposing
    ( Objective
    ,  ObjectiveId
       -- , new

    )

import Patron
import Util


type Magic
    = Magic (List Skill)


type Objective
    = Idea
    | Summons Magic Incantation



-- | Journey Magic Incantaion
-- | Quest Magic


type alias Idea =
    { id : IdeaId
    , description : Name
    , creator : Account
    }


type IdeaId
    = IdeaId (Maybe Int)
