module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (height, style, type_, value, width)
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

        dyad =
            pickDyad model.color
    in
    div
        []
        [ input
            [ type_ "color"
            , value colorHex
            , onInput OnChange
            ]
            []
        , text colorHex
        , div [] (viewColorSet dyad)
        ]


viewColorSet : List Color -> List (Html msg)
viewColorSet colors =
    List.map viewColor colors


viewColor : Color -> Html msg
viewColor color =
    span
        [ style "background-color" (Color.Convert.colorToHex color) ]
        [ text "\u{3000}\u{3000}\u{3000}\u{3000}" ]


pickDyad : Color -> List Color
pickDyad baseColor =
    let
        nextColor =
            pickNthNext baseColor 1 2
    in
    case nextColor of
        Ok color ->
            baseColor :: [ color ]

        Err _ ->
            [ baseColor ]


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
        |. symbol "%,"
        |. spaces
        |= float
        |. symbol "%)"


pickNthNext : Color -> Int -> Int -> Result String Color
pickNthNext baseColor n total =
    let
        hueDifferenceUnit =
            1 / toFloat total

        baseColorHslInDegree =
            baseColor
                |> Color.Convert.colorToCssHsl
                |> Parser.run hsl

        baseColorHsl =
            case baseColorHslInDegree of
                Ok colorHsl ->
                    Ok
                        {- Change HSL formart from {h: 0-360[deg], s: 0-100[%], l: 0-100[%]} to {h: 0-1, s: 0-1, l: 0-1} -}
                        { colorHsl
                            | h = colorHsl.h / 360
                            , s = colorHsl.s / 100
                            , l = colorHsl.l / 100
                        }

                Err msg ->
                    Err msg
    in
    case baseColorHsl of
        Ok colorHsl ->
            let
                gainedHue =
                    colorHsl.h + toFloat n * hueDifferenceUnit

                pickedHue =
                    if gainedHue >= 1 then
                        gainedHue - 1

                    else
                        gainedHue
            in
            Ok <| Color.hsl pickedHue colorHsl.s colorHsl.l

        Err _ ->
            Err "Failed pickNthNext"
