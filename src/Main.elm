module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
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
    { color : Color }


defaultColor : Color
defaultColor =
    Color.white


init : () -> ( Model, Cmd Msg )
init _ =
    ( { color = defaultColor }, Cmd.none )


type Msg
    = OnChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( OnChange colorHex, _ ) ->
            ( { color =
                    case Color.Convert.hexToColor colorHex of
                        Ok color ->
                            color

                        Err _ ->
                            model.color
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        colorHex =
            Color.Convert.colorToHex model.color
    in
    div []
        [ input
            [ type_ "color"
            , value colorHex
            , onInput OnChange
            ]
            []
        , text colorHex
        ]
