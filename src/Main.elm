module Main exposing (main)

import Browser
import Color exposing (Color)
import Color.Convert exposing (colorToCssHsl, colorToCssRgb, colorToHex)
import Element exposing (Element, column, el, html, htmlAttribute, layout, row, text, width, height, fill, px, centerX, spacing)
import Element.Background as Background
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser exposing ((|.), (|=), Parser, float, spaces, succeed, symbol)
import Element.Border as Border


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


defaultElmUIColor : Element.Color
defaultElmUIColor =
    Element.rgb 1 1 1


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
    layout
        []
        (column
            [ width fill ]
            [ viewHeader
            , row
                [ width fill
                , height fill
                ]
                [ viewLeftPane model
                , viewMainPane model
                ]
            ]
        )
        

viewHeader : Element msg
viewHeader =
    row
        [ width fill
        , spacing 10
        , Border.width 1
        ]
        [ el [ centerX ] <| text "Logo"
        , el [ centerX ] <| text "ColorStew"
        ]


viewLeftPane : Model -> Element Msg
viewLeftPane model =
    column
        [ width <| px 350
        , height fill
        , Border.width 1
        ]
        [ row
            [ width fill
            , spacing 10
            , Border.width 1
            ]
            [ text "PickMainColor"
            , html
                (Html.input
                    [ Attributes.type_ "color"
                    , Attributes.value <| Color.Convert.colorToHex model.color
                    , Events.onInput OnChange
                    ]
                    []
                )
            ]
        , row
            [ width fill
            , spacing 10
            , Border.width 1
            ]
            [ text "ColorShemes"
            , text "Filter"
            ]
        , viewColorScheme "Dyad" <| pickDyad model.color
        ]


viewColorScheme : String -> List Color -> Element msg
viewColorScheme scheme colors =
    column
        [ spacing 10 ]
        [ text scheme
        , viewColorSet colors
        ]

viewColorSet : List Color -> Element msg
viewColorSet colors =
    colors
        |> List.map toElmUIColor
        |> List.map viewColor
        |> row []


viewColor : Element.Color -> Element msg
viewColor color =
    el
        [ Background.color color ]
        (text "\u{3000}\u{3000}\u{3000}\u{3000}")


viewMainPane : Model -> Element msg
viewMainPane model =
    let
        stewedColor : Color
        stewedColor = Color.darkPurple -- provisional
    in
    column
        [ width fill
        , Border.width 1
        ]
        [ el [ centerX ] <| text "PREVIEW AREA"
        , row
            [ width fill ]
            [ viewStewedColor stewedColor 0 0 
            , viewStewedColor stewedColor 0 0
            ]
        ]


viewStewedColor : Color -> Float -> Float -> Element msg
viewStewedColor color saturation lightness =
    column
       [ width fill
       , Border.width 1
       ]
       [ row
           [ centerX
           , spacing 10
           ]
           [ text "#Hex"
           , text "Copy"
           ]
       , el [ centerX ] <| text "Color"
       , el [ centerX ] <| text "SaturationSlider"
       , el [ centerX ] <| text "LightnessSlider"
       ]


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


toElmUIColor : Color -> Element.Color
toElmUIColor color =
    let
        colorRgb_ : Result (List Parser.DeadEnd) Rgb
        colorRgb_ =
            color
                |> colorToCssRgb
                |> Parser.run rgb
    in
    case colorRgb_ of
        Ok colorRgb ->
            Element.rgb (colorRgb.r / 255) (colorRgb.g / 255) (colorRgb.b / 255)

        Err _ ->
            defaultElmUIColor


type alias Rgb =
    { r : Float
    , g : Float
    , b : Float
    }


rgb : Parser Rgb
rgb =
    succeed Rgb
        |. symbol "rgb("
        |= float
        |. symbol ","
        |. spaces
        |= float
        |. symbol ","
        |. spaces
        |= float
        |. symbol ")"


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

        baseColorHslWithDegreeHue =
            baseColor
                |> Color.Convert.colorToCssHsl
                |> Parser.run hsl

        baseColorHsl =
            case baseColorHslWithDegreeHue of
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
