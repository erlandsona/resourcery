module Skill exposing
    ( Magic
    , Skill
    , SkillId
    , new
    )

import Util


type alias Skill =
    { id : SkillId
    , name : Name
    }


type SkillId
    = SkillId (Maybe Int)


new : Name -> Skill
new =
    Skill Nothing
