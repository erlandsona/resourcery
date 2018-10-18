module Skills exposing
    ( Skill
    ,  SkillId
       -- , new

    )


type alias Skill =
    { id : SkillId
    , name : String
    }


type SkillId
    = SkillId (Maybe Int)
