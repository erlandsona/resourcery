module Magic exposing
    ( Magic
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


type Magic
    = Skill (Set String)


new : List Magic -> Magic
new =
    List.foldl (\(Skill setA) (Skill setB) -> Skill <| Set.union setA setB) (Skill Set.empty)


merge : Magic -> Magic -> Magic
merge (Skill setA) (Skill setB) =
    Skill <| Set.union setA setB


ruby : Magic
ruby =
    Skill <| Set.singleton "ruby"


php : Magic
php =
    Skill <| Set.singleton "PHP"


oop : Magic
oop =
    Skill <| Set.singleton "Object Oriented Web Languages"


devOps : Magic
devOps =
    Skill <| Set.singleton "DevOps"


editCode : Magic
editCode =
    Skill <| Set.singleton "edit code"


git : Magic
git =
    Skill <| Set.singleton "git"
