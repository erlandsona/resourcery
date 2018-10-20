module Skill exposing
    ( Skill
    , new
    )


type Skill
    = Skill (Maybe Int) Name


type Name
    = Name String


new : Name -> Skill
new =
    Skill Nothing
