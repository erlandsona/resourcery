module Account.Name exposing
    ( Name
    ,  new
       -- , show

    )

-- import Account exposing (Account)


type Name
    = Name String


new : String -> Name
new =
    Name



-- show : Account -> String
-- show (Name str) =
--     str
