module Idea.Description exposing
    ( Description
    , new
    , show
    )


type Description
    = Description String


new : String -> Description
new =
    Description


show : Description -> String
show (Description s) =
    s
