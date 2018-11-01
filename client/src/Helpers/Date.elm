module Helpers.Date exposing (cal)

import Time exposing (Month(..), Posix, utc)
import Time.Extra as TimE exposing (Parts)


cal : Int -> Month -> Int -> Posix
cal y m d =
    Parts y m d 0 0 0 0
        |> TimE.partsToPosix utc
