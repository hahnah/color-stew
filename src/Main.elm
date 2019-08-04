port module Main exposing (main)

import Array exposing (Array)
import Browser
import Color exposing (Color)
import Color.Convert exposing (colorToCssHsl, colorToCssRgb, colorToHex)
import DnDList
import Element exposing (Element, alignTop, centerX, column, el, fill, height, html, htmlAttribute, inFront, layout, none, paddingEach, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button, defaultThumb, labelHidden, slider)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Logo
import Parser exposing ((|.), (|=), DeadEnd, Parser, float, spaces, succeed, symbol)


main : Program () Model Msg
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
    , dnd : DnDList.Model -- dnd stands for Drag and Drop
    }


dndSystem : DnDList.System Color Msg
dndSystem =
    let
        dndConfig : DnDList.Config Color
        dndConfig =
            { beforeUpdate = \_ _ list -> list
            , movement = DnDList.Free
            , listen = DnDList.OnDrag
            , operation = DnDList.Rotate
            }
    in
    DnDList.create dndConfig DragAndDrop


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
      , dnd = dndSystem.model
      }
    , Cmd.none
    )


type Msg
    = PickColor String
    | SelectScheme (List Color)
    | AdjustSaturation Int Float
    | AdjustLightness Int Float
    | DragAndDrop DnDList.Msg
    | CopyColorCode String
    | None Float


type HslElement
    = Hue
    | Saturation
    | Lightness


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickColor colorHex ->
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

        SelectScheme schemeColors ->
            ( { model | stewedColors = schemeColors }
            , Cmd.none
            )

        AdjustSaturation index saturation ->
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

        AdjustLightness index lightness ->
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

        DragAndDrop dndMsg ->
            let
                ( dnd, colors ) =
                    dndSystem.update dndMsg model.dnd model.stewedColors
            in
            ( { model | stewedColors = colors, dnd = dnd }
            , dndSystem.commands model.dnd
            )

        CopyColorCode colorCode ->
            ( model, copyString colorCode )

        None _ ->
            ( model, Cmd.none )


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
    dndSystem.subscriptions model.dnd


port copyString : String -> Cmd msg


view : Model -> Html Msg
view model =
    layout
        [ width fill
        , alignTop
        , inFront (viewGhostStewedColor model.dnd model.stewedColors)
        ]
        (row
            [ width fill
            ]
            [ viewLeftPane model
            , viewMainPane model
            ]
        )


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
        , viewColorScheme "Split Complementary" <| pickSplitComplementary model.pickedColor
        , viewColorScheme "Tetrad" <| pickTetrad model.pickedColor
        , viewColorScheme "Pentad" <| pickPentad model.pickedColor
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
    column
        [ width fill
        , Border.width 1
        ]
        [ viewPreview model
        , row
            [ width fill ]
            (model.stewedColors
                |> List.indexedMap (viewRealStewedColor model.dnd)
            )
        ]


viewPreview : Model -> Element Msg
viewPreview model =
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

        titleColor : Color
        titleColor =
            model.stewedColors
                |> List.drop 2
                |> List.head
                |> Maybe.withDefault logoColor

        textColor : Color
        textColor =
            model.stewedColors
                |> List.drop 3
                |> List.head
                |> Maybe.withDefault backgroundColor

        backgroundColor2 : Color
        backgroundColor2 =
            model.stewedColors
                |> List.drop 4
                |> List.head
                |> Maybe.withDefault logoColor
    in
    column
        [ centerX
        , Background.color <| toElmUIColor backgroundColor
        , width fill
        ]
        [ el
            [ Font.color <| toElmUIColor titleColor
            , Font.size 30
            , centerX
            , paddingEach
                { top = 10
                , right = 0
                , bottom = 0
                , left = 0
                }
            ]
            (text "Color Stew")
        , el
            [ Font.color <| toElmUIColor backgroundColor
            , centerX
            ]
            (html <| Logo.logo 400 logoColor)
        , el
            [ Background.color <| toElmUIColor backgroundColor2
            , width fill
            ]
            (el
                [ Font.color <| toElmUIColor textColor
                , centerX
                ]
                (paragraph
                    []
                    [ text "Color Stew is a color combination experiment tool."
                    , text " Text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text."
                    ]
                )
            )
        ]


viewRealStewedColor : DnDList.Model -> Int -> Color -> Element Msg
viewRealStewedColor dndModel index color =
    let
        colorId : String
        colorId =
            "stewedColor-" ++ String.fromInt index

        attributesForDndHandling : List (Element.Attribute Msg)
        attributesForDndHandling =
            case dndSystem.info dndModel of
                Just { dragIndex } ->
                    if dragIndex /= index then
                        htmlAttribute (Attributes.id colorId) :: List.map htmlAttribute (dndSystem.dropEvents index colorId)

                    else
                        [ htmlAttribute (Attributes.id colorId) ]

                Nothing ->
                    htmlAttribute (Attributes.id colorId) :: List.map htmlAttribute (dndSystem.dragEvents index colorId)
    in
    viewStewedColorWithSurroundings attributesForDndHandling index color


viewGhostStewedColor : DnDList.Model -> List Color -> Element.Element Msg
viewGhostStewedColor dndModel colors =
    let
        maybeDragColor : Maybe Color
        maybeDragColor =
            dndSystem.info dndModel
                |> Maybe.andThen (\{ dragIndex } -> colors |> List.drop dragIndex |> List.head)

        attributesForDndHandling : List (Element.Attribute Msg)
        attributesForDndHandling =
            List.map htmlAttribute (dndSystem.ghostStyles dndModel)
    in
    case maybeDragColor of
        Just color ->
            viewStewedColor attributesForDndHandling color

        Nothing ->
            Element.none


viewStewedColorWithSurroundings : List (Element.Attribute Msg) -> Int -> Color -> Element Msg
viewStewedColorWithSurroundings attributesForDndHandling index color =
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
                    , button
                        []
                        { onPress = Just <| CopyColorCode <| Color.Convert.colorToHex color
                        , label = text "📋"
                        }
                    ]
                , viewStewedColor attributesForDndHandling color
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


viewStewedColor : List (Element.Attribute Msg) -> Color -> Element Msg
viewStewedColor attributesForDndHandling color =
    el
        (List.append
            [ centerX
            , width <| px 100
            , height <| px 70
            , Background.color <| toElmUIColor color
            ]
            attributesForDndHandling
        )
        Element.none


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


pickSplitComplementary : Color -> List Color
pickSplitComplementary color =
    case ( pickNthNext color 12 5, pickNthNext color 12 7 ) of
        ( Ok color1, Ok color2 ) ->
            color :: color1 :: color2 :: []

        ( Ok color1, Err _ ) ->
            color :: color1 :: []

        ( Err _, Ok color2 ) ->
            color :: color2 :: []

        ( Err _, Err _ ) ->
            color :: []


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
