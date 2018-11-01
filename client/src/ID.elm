module ID exposing
    ( ID
    , gen
    , new
    ,  nil
       -- , value

    )

import Random exposing (Generator, Seed)
import UUID exposing (UUID)



-- Should be type ID = ID UUID
-- once we get comparables for any type
-- this can be updated.


type alias ID =
    String


gen : Seed -> ID
gen =
    Random.step UUID.generator
        >> Tuple.first
        >> UUID.canonical


new : String -> ID
new str =
    god
        |> UUID.childNamed str
        |> UUID.canonical


nil : ID
nil =
    UUID.canonical god



-- value : ID -> String
-- value (ID id) =
--     UUID.canonical id
{-
   Private
-}


god : UUID
god =
    UUID.nil
