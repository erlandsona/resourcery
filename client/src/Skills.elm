module Skills exposing
    ( Skills
    , devOps
    , editCode
    , git
    , merge
    , new
    , oop
    , php
    , ruby
    )

import Set exposing (Set)


type Skills
    = Skill (Set String)


new : List Skills -> Skills
new =
    List.foldl (\(Skill setA) (Skill setB) -> Skill <| Set.union setA setB) (Skill Set.empty)


merge : Skills -> Skills -> Skills
merge (Skill setA) (Skill setB) =
    Skill <| Set.union setA setB


ruby : Skills
ruby =
    Skill <| Set.singleton "ruby"


php : Skills
php =
    Skill <| Set.singleton "PHP"


oop : Skills
oop =
    Skill <| Set.singleton "Object Oriented Web Languages"


devOps : Skills
devOps =
    Skill <| Set.singleton "DevOps"


editCode : Skills
editCode =
    Skill <| Set.singleton "edit code"


git : Skills
git =
    Skill <| Set.singleton "git"
