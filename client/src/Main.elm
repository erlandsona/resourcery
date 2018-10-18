module Main exposing (Model(..))

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type Model
    = Charging
    | Failed
    | Charged



-- main =
--     Browser.sandbox { init = 0, update = update, view = view }
-- type Msg
--     = Increment
--     | Decrement
-- update msg model =
--     case msg of
--         Increment ->
--             model + 1
--         Decrement ->
--             model - 1
-- view model =
--     div []
--         [ button [ onClick Decrement ] [ text "-" ]
--         , div [] [ text (String.fromInt model) ]
--         , button [ onClick Increment ] [ text "+" ]
--         ]
-- type Model
--     = Redirect Session
--     | NotFound Session
--     | Home Home.Model
--     | Settings Settings.Model
--     | Login Login.Model
--     | Register Register.Model
--     | Profile Username Profile.Model
--     | Article Article.Model
--     | Editor (Maybe Slug) Editor.Model
