module Main exposing (main)

import Array exposing (Array)
import Browser
import Color exposing (Color)
import Color.Convert exposing (colorToCssHsl, colorToCssRgb, colorToHex)
import Element exposing (Element, centerX, column, el, fill, height, html, htmlAttribute, layout, none, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Input exposing (defaultThumb, labelHidden, slider)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Logo
import Parser exposing ((|.), (|=), DeadEnd, Parser, float, spaces, succeed, symbol)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { pickedColor : Color
    , stewedColors : List Color
    }


defaultColor : Color
defaultColor =
    Color.white


defaultElmUIColor : Element.Color
defaultElmUIColor =
    Element.rgb 1 1 1


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pickedColor = defaultColor
      , stewedColors = []
      }
    , Cmd.none
    )


type Msg
    = PickColor String
    | SelectScheme (List Color)
    | AdjustSaturation Int Float
    | AdjustLightness Int Float


type HslElement
    = Hue
    | Saturation
    | Lightness


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( PickColor colorHex, _ ) ->
            ( { model
                | pickedColor =
                    case Color.Convert.hexToColor colorHex of
                        Ok color ->
                            color

                        Err _ ->
                            model.pickedColor
              }
            , Cmd.none
            )

        ( SelectScheme schemeColors, _ ) ->
            ( { model | stewedColors = schemeColors }
            , Cmd.none
            )

        ( AdjustSaturation index saturation, _ ) ->
            let
                arrayedColors : Array Color
                arrayedColors =
                    Array.fromList model.stewedColors

                colorToBeAdjusted : Maybe Color
                colorToBeAdjusted =
                    arrayedColors
                        |> Array.get index

                adjustedColor : Result (List DeadEnd) Color
                adjustedColor =
                    adjustColor model Saturation index saturation

                adjustedColors : List Color
                adjustedColors =
                    case adjustedColor of
                        Ok color ->
                            Array.set index color arrayedColors
                                |> Array.toList

                        Err _ ->
                            model.stewedColors
            in
            ( { model | stewedColors = adjustedColors }
            , Cmd.none
            )

        ( AdjustLightness index lightness, _ ) ->
            let
                arrayedColors : Array Color
                arrayedColors =
                    Array.fromList model.stewedColors

                colorToBeAdjusted : Maybe Color
                colorToBeAdjusted =
                    arrayedColors
                        |> Array.get index

                adjustedColor : Result (List DeadEnd) Color
                adjustedColor =
                    adjustColor model Lightness index lightness

                adjustedColors : List Color
                adjustedColors =
                    case adjustedColor of
                        Ok color ->
                            Array.set index color arrayedColors
                                |> Array.toList

                        Err _ ->
                            model.stewedColors
            in
            ( { model | stewedColors = adjustedColors }
            , Cmd.none
            )


adjustColor : Model -> HslElement -> Int -> Float -> Result (List DeadEnd) Color
adjustColor model adjustingElement index value =
    let
        arrayedColors : Array Color
        arrayedColors =
            Array.fromList model.stewedColors

        colorToBeAdjusted : Maybe Color
        colorToBeAdjusted =
            arrayedColors
                |> Array.get index
    in
    case colorToBeAdjusted of
        Just color ->
            color
                |> Color.Convert.colorToCssHsl
                |> Parser.run hsl
                |> (\resultHsl ->
                        case resultHsl of
                            Ok hsl_ ->
                                (case adjustingElement of
                                    Hue ->
                                        { hsl_ | h = value }

                                    Saturation ->
                                        { hsl_ | s = value }

                                    Lightness ->
                                        { hsl_ | l = value }
                                )
                                    -- Change HSL formart from {h: 0-360[deg], s: 0-100[%], l: 0-100[%]} to {h: 0-1, s: 0-1, l: 0-1}
                                    |> (\hsl__ -> Color.hsl (hsl__.h / 360) (hsl__.s / 100) (hsl__.l / 100))
                                    |> Ok

                            Err message ->
                                Err message
                   )

        Nothing ->
            Err [ DeadEnd 1 index <| Parser.Problem "Failed to get an element from an array." ]


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
                    , Attributes.value <| Color.Convert.colorToHex model.pickedColor
                    , Events.onInput PickColor
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
        , viewColorScheme "Dyad" <| pickDyad model.pickedColor
        , viewColorScheme "Triad" <| pickTriad model.pickedColor
        , viewColorScheme "Tetrad" <| pickTetrad model.pickedColor
        , viewColorScheme "Pentad" <| pickPentad model.pickedColor
        , viewColorScheme "Hexad" <| pickHexad model.pickedColor
        ]


viewColorScheme : String -> List Color -> Element Msg
viewColorScheme scheme colors =
    column
        [ onClick <| SelectScheme colors
        , spacing 10
        ]
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
        [ Background.color color
        , width <| px 56
        , height <| px 22
        ]
        none


viewMainPane : Model -> Element Msg
viewMainPane model =
    let
        logoColor : Color
        logoColor =
            model.stewedColors
                |> List.head
                |> Maybe.withDefault Color.black
        
        backgroundColor : Color
        backgroundColor =
            model.stewedColors
                |> List.tail
                |> Maybe.withDefault []
                |> List.head
                |> Maybe.withDefault Color.white
    in
    column
        [ width fill
        , Border.width 1
        ]
        [ el
            [ centerX
            , Background.color <| toElmUIColor backgroundColor
            ]
            (html <| Logo.logo 400 logoColor)
        , row
            [ width fill ]
            (model.stewedColors
                |> Array.fromList
                |> Array.indexedMap viewStewedColor
                |> Array.toList
            )
        ]


viewStewedColor : Int -> Color -> Element Msg
viewStewedColor index color =
    let
        colorHsl_ : Result (List DeadEnd) Hsl
        colorHsl_ =
            color
                |> Color.Convert.colorToCssHsl
                |> Parser.run hsl
    in
    case colorHsl_ of
        Ok colorHsl ->
            column
                [ width fill
                , Border.width 1
                ]
                [ row
                    [ centerX
                    , spacing 10
                    ]
                    [ text <| Color.Convert.colorToHex color
                    , text "Copy"
                    ]
                , el
                    [ centerX
                    , width <| px 100
                    , height <| px 70
                    , Background.color <| toElmUIColor color
                    ]
                    Element.none
                , el
                    [ centerX
                    , width <| px 100
                    ]
                    (slider
                        []
                        { label = labelHidden <| String.fromFloat colorHsl.s
                        , onChange = AdjustSaturation index
                        , min = 0
                        , max = 100
                        , step = Nothing
                        , value = colorHsl.s
                        , thumb = defaultThumb -- TODO: Replace with a saturation icon
                        }
                    )
                , el
                    [ centerX
                    , width <| px 100
                    ]
                    (slider
                        []
                        { label = labelHidden <| String.fromFloat colorHsl.l
                        , onChange = AdjustLightness index
                        , min = 0
                        , max = 100
                        , step = Nothing
                        , value = colorHsl.l
                        , thumb = defaultThumb -- TODO: Replace with a lightness icon
                        }
                    )
                ]

        Err _ ->
            column
                [ width fill
                , Border.width 1
                ]
                [ row
                    [ centerX
                    , spacing 10
                    ]
                    [ text "#??????"
                    , text "Copy"
                    ]
                , el
                    [ centerX
                    , width <| px 100
                    , height <| px 70
                    ]
                    (text "Error")
                , el [ centerX ] <| text "SaturationSlider"
                , el [ centerX ] <| text "LightnessSlider"
                ]


pickPolyad : Color -> Int -> List Color
pickPolyad baseColor dimension =
    List.range 0 (dimension - 1)
        |> List.map (pickNthNext baseColor dimension)
        |> List.foldr
            (\resultColor ->
                \acc ->
                    case resultColor of
                        Ok color ->
                            color :: acc

                        Err _ ->
                            acc
            )
            []


pickDyad : Color -> List Color
pickDyad baseColor =
    pickPolyad baseColor 2


pickTriad : Color -> List Color
pickTriad baseColor =
    pickPolyad baseColor 3


pickTetrad : Color -> List Color
pickTetrad baseColor =
    pickPolyad baseColor 4


pickPentad : Color -> List Color
pickPentad baseColor =
    pickPolyad baseColor 5


pickHexad : Color -> List Color
pickHexad baseColor =
    pickPolyad baseColor 6


toElmUIColor : Color -> Element.Color
toElmUIColor color =
    let
        colorRgb_ : Result (List DeadEnd) Rgb
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


pickNthNext : Color -> Int -> Int -> Result (List DeadEnd) Color
pickNthNext baseColor total n =
    let
        hueDifferenceUnit : Float
        hueDifferenceUnit =
            1 / toFloat total

        baseColorHslWithDegreeHue : Result (List DeadEnd) Hsl
        baseColorHslWithDegreeHue =
            baseColor
                |> Color.Convert.colorToCssHsl
                |> Parser.run hsl

        baseColorHsl : Result (List DeadEnd) Hsl
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
            Err [ DeadEnd 1 1 <| Parser.Problem "Failed pickNthNext" ]
