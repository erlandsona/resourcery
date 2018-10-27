module Main exposing (Model, Msg(..), init, main, myElement, myRowOfStuff, nothing, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Events exposing (onClick)
import Types
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    Maybe String


nothing : ( Model, Cmd Msg )
nothing =
    ( Nothing, Cmd.none )



-- INIT


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    nothing



-- VIEW


view : Model -> Document Msg
view _ =
    { title = "Resourcery"
    , body =
        [ Element.layout []
            myRowOfStuff
        ]
    }


myRowOfStuff =
    row [ width fill, centerY, spacing 30 ]
        [ myElement
        , myElement
        , el [ alignRight ] myElement
        ]


myElement : Element msg
myElement =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            nothing

        ChangedUrl url ->
            nothing

        Increment ->
            nothing

        Decrement ->
            nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [-- scroll ScrollBar
         -- , scrollStart Stop
         -- , setScrollTargets SetPageTops
        ]
