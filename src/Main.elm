module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { colorCode : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { colorCode = "#ffffff" }, Cmd.none )


type Msg
    = OnChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( OnChange colorCode_, _ ) ->
            ( { colorCode = colorCode_ }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "color"
            , value model.colorCode
            , onInput OnChange
            ]
            []
        , text model.colorCode
        ]
