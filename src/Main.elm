module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput)
import Parser exposing ((|.), (|=), Parser, float, spaces, succeed, symbol)


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
    div
        [ style "color" "#ffffff"
        , style "background-color" "#123456"
        ]
        [ input
            [ type_ "color"
            , value colorHex
            , onInput OnChange
            ]
            []
        , text colorHex
        ]

{- progressing
pickDyad : Color -> List Color
pickDyad baseColor =
    let
        baseColorHsl =
            baseColor
                |> Color.Convert.colorToCssHsl
                |> Parser.run hsl
    in
    case baseColorHsl of
        Ok colorHsl ->
            baseColor :: [ Color.hsl (abs (1 - colorHsl.h)) colorHsl.s colorHsl.l ]

        Err _ ->
            [ baseColor ]
-}

type alias Hsl =
    { h : Float
    , s : Float
    , l : Float
    }


hsl : Parser Hsl
hsl =
    succeed Hsl
        |. symbol "hsl("
        |. spaces
        |= float
        |. symbol ","
        |. spaces
        |= float
        |. symbol ","
        |. spaces
        |= float
        |. symbol ")"

pickNthNext : Color -> Int -> Int -> Result String Color
pickNthNext baseColor n total =
    let
        hueDifferenceUnit = 1.0 / (toFloat total)
        baseColorHsl =
            baseColor
                |> Color.Convert.colorToCssHsl
                |> Parser.run hsl
    in
        case baseColorHsl of
                Ok colorHsl ->
                    let
                        gainedHue = colorHsl.h + (toFloat n) * hueDifferenceUnit
                        pickedHue = gainedHue - (toFloat (floor gainedHue))
                    in
                        Ok <| Color.hsl pickedHue colorHsl.s colorHsl.l

                Err _ ->
                    Err "Failed pickNthNext"