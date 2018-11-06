module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Events exposing (onClick)
import Url exposing (Url)
import World exposing (World)



-- MAIN


main : Program () World Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- INIT


init : flags -> Url -> Key -> ( World, Cmd Msg )
init flags url key =
    let
        world : World
        world =
            { incantations = Dict.empty
            , magic = Dict.empty
            , patrons = Dict.empty
            , sourcerers = Dict.empty
            , summons = Dict.empty
            }
    in
    ( world, Cmd.none )



-- VIEW


view : World -> Document Msg
view _ =
    { title = "Resourcery"
    , body =
        [ Element.layout []
            myRowOfStuff
        ]
    }


myRowOfStuff =
    column [ width fill ]
        [ header
        , body
        ]


header =
    row
        [ Background.color (rgb255 0 0 0)
        , Font.color (rgb255 255 255 255)
        , width fill
        , alignTop
        , height (px 50)
        ]
        [ el [] (text "Resourcery")
        ]


body : Element msg
body =
    el
        [ Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        ]
        (text "stylish!")



-- Update


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | Increment
    | Decrement


update : Msg -> World -> ( World, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            ( model, Cmd.none )

        ChangedUrl url ->
            ( model, Cmd.none )

        Increment ->
            ( model, Cmd.none )

        Decrement ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : World -> Sub Msg
subscriptions _ =
    Sub.batch
        [-- scroll ScrollBar
         -- , scrollStart Stop
         -- , setScrollTargets SetPageTops
        ]
