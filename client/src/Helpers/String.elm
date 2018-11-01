module Helpers.String exposing (concat)

import Set exposing (Set)


concat : Set String -> String
concat =
    Set.foldl (++) ""
