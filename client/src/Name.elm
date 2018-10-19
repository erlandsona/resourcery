module Name exposing (Name, new, show)


type Name
    = Name String


new : String -> Name
new =
    Name


show : Name -> String
show (Name str) =
    str
