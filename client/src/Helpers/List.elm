module Helpers.List exposing (sum)


sum : (a -> number) -> List a -> number
sum f =
    List.foldl (f >> (+)) 0
