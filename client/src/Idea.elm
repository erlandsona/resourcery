module Idea exposing
    ( Id
    , Idea
    , new
    )

import Account exposing (Account)
import Idea.Description as Description exposing (Description)


type Idea
    = Idea Info


type alias Info =
    { id : Id
    , description : Description
    , creator : Account
    }


type Id
    = Id (Maybe Int)


new : String -> Account -> Idea
new desc =
    Info (Id Nothing) (Description.new desc)
        >> Idea
