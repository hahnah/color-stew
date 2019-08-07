port module Main exposing (main)

import Array exposing (Array)
import Browser
import Color exposing (Color)
import Color.Convert exposing (colorToCssHsl, colorToCssRgb, colorToHex)
import DnDList
import Element exposing (Element, alignTop, centerX, column, el, fill, height, html, htmlAttribute, inFront, layout, none, paddingEach, paddingXY, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input exposing (button, defaultThumb, labelHidden, slider, thumb)
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
    , selectedColorScheme : ColorScheme
    , hoveredColorScheme : Maybe ColorScheme
    , indexOfHoveredStewedColor : Maybe Int
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
      , selectedColorScheme = Monochromatic
      , hoveredColorScheme = Nothing
      , indexOfHoveredStewedColor = Nothing
      , dnd = dndSystem.model
      }
    , Cmd.none
    )


type Msg
    = PickColor String
    | SelectScheme ColorScheme (List Color)
    | AdjustSaturation Int Float
    | AdjustLightness Int Float
    | DragAndDrop DnDList.Msg
    | CopyColorCode String
    | EnterMouseOntoColorScheme ColorScheme
    | LeaveMouseFromColorScheme ColorScheme
    | EnterMouseOntoStewedColor Int
    | LeaveMouseFromStewedColor Int
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

        SelectScheme scheme schemeColors ->
            ( { model
                | stewedColors = schemeColors
                , selectedColorScheme = scheme
              }
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

        EnterMouseOntoColorScheme ontoColorScheme ->
            ( { model | hoveredColorScheme = Just ontoColorScheme }
            , Cmd.none
            )

        LeaveMouseFromColorScheme fromColorScheme ->
            ( { model
                | hoveredColorScheme =
                    case model.hoveredColorScheme of
                        Just hoveredColorScheme ->
                            if fromColorScheme == hoveredColorScheme then
                                Nothing

                            else
                                model.hoveredColorScheme

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )

        EnterMouseOntoStewedColor ontoIndex ->
            ( { model | indexOfHoveredStewedColor = Just ontoIndex }
            , Cmd.none
            )

        LeaveMouseFromStewedColor fromIndex ->
            ( { model
                | indexOfHoveredStewedColor =
                    case model.indexOfHoveredStewedColor of
                        Just index ->
                            if fromIndex == index then
                                Nothing

                            else
                                model.indexOfHoveredStewedColor

                        Nothing ->
                            model.indexOfHoveredStewedColor
              }
            , Cmd.none
            )

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


type ColorScheme
    = Dyad
    | DyadPlusDarkAndLight
    | Triad
    | TriadPlusDarkAndLight
    | SplitComplementary
    | SplitComplementaryPlusDarkAndLight
    | Tetrad
    | TetradPlusDark
    | TetradPlusLight
    | Pentad
    | Monochromatic


colorSchemeToString : ColorScheme -> String
colorSchemeToString scheme =
    case scheme of
        Monochromatic ->
            "Monochromatic"

        Dyad ->
            "Dyad"

        DyadPlusDarkAndLight ->
            "Dyad + Dark & Light"

        Triad ->
            "Triad"

        TriadPlusDarkAndLight ->
            "Triad + Dark & Light"

        SplitComplementary ->
            "Split Complementary"

        SplitComplementaryPlusDarkAndLight ->
            "Split Complementary + Dark & Light"

        Tetrad ->
            "Tetrad"

        TetradPlusDark ->
            "Tetrad + Dark"

        TetradPlusLight ->
            "Tetrad + Light"

        Pentad ->
            "Pentad"


pickSchemedColors : ColorScheme -> Color -> List Color
pickSchemedColors scheme baseColor =
    case scheme of
        Monochromatic ->
            pickMonochromatic baseColor

        Dyad ->
            pickDyad baseColor

        DyadPlusDarkAndLight ->
            pickDyad baseColor ++ [ pickDarkColor baseColor ] ++ [ pickLightColor baseColor ]

        Triad ->
            pickTriad baseColor

        TriadPlusDarkAndLight ->
            pickTriad baseColor ++ [ pickDarkColor baseColor ] ++ [ pickLightColor baseColor ]

        SplitComplementary ->
            pickSplitComplementary baseColor

        SplitComplementaryPlusDarkAndLight ->
            pickSplitComplementary baseColor ++ [ pickDarkColor baseColor ] ++ [ pickLightColor baseColor ]

        Tetrad ->
            pickTetrad baseColor

        TetradPlusDark ->
            pickTetrad baseColor ++ [ pickDarkColor baseColor ]

        TetradPlusLight ->
            pickTetrad baseColor ++ [ pickLightColor baseColor ]

        Pentad ->
            pickPentad baseColor


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
        , viewColorScheme Monochromatic model
        , viewColorScheme Dyad model
        , viewColorScheme DyadPlusDarkAndLight model
        , viewColorScheme Triad model
        , viewColorScheme TriadPlusDarkAndLight model
        , viewColorScheme SplitComplementary model
        , viewColorScheme SplitComplementaryPlusDarkAndLight model
        , viewColorScheme Tetrad model
        , viewColorScheme TetradPlusDark model
        , viewColorScheme TetradPlusLight model
        , viewColorScheme Pentad model
        ]


viewColorScheme : ColorScheme -> Model -> Element Msg
viewColorScheme scheme model =
    let
        schemeName : String
        schemeName =
            colorSchemeToString scheme

        schemedColors : List Color
        schemedColors =
            pickSchemedColors scheme model.pickedColor

        backgroundColor : Color
        backgroundColor =
            if scheme == model.selectedColorScheme then
                Color.gray

            else
                case model.hoveredColorScheme of
                    Just hoveredColorScheme ->
                        if scheme == hoveredColorScheme then
                            Color.lightGray

                        else
                            Color.white

                    Nothing ->
                        Color.white
    in
    column
        [ onClick <| SelectScheme scheme schemedColors
        , onMouseLeave <| LeaveMouseFromColorScheme scheme
        , onMouseEnter <| EnterMouseOntoColorScheme scheme
        , spacing 10
        , width fill
        , Background.color <| toElmUIColor backgroundColor
        ]
        [ text schemeName
        , viewColorSet schemedColors
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
                |> List.indexedMap (viewRealStewedColor model)
            )
        ]


viewPreview : Model -> Element Msg
viewPreview model =
    let
        baseColor : Color
        baseColor =
            model.stewedColors
                |> List.head
                |> Maybe.withDefault Color.black

        logoColor : Color
        logoColor =
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

        textBackgroundColor : Color
        textBackgroundColor =
            model.stewedColors
                |> List.drop 3
                |> List.head
                |> Maybe.withDefault logoColor

        textColor : Color
        textColor =
            model.stewedColors
                |> List.drop 4
                |> List.head
                |> Maybe.withDefault baseColor
    in
    column
        [ centerX
        , Background.color <| toElmUIColor baseColor
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
            [ Font.color <| toElmUIColor baseColor
            , centerX
            ]
            (html <| Logo.logo 400 logoColor)
        , el
            [ Background.color <| toElmUIColor textBackgroundColor
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


viewRealStewedColor : Model -> Int -> Color -> Element Msg
viewRealStewedColor model index color =
    let
        colorId : String
        colorId =
            "stewedColor-" ++ String.fromInt index

        attributesForDndHandling : List (Element.Attribute Msg)
        attributesForDndHandling =
            case dndSystem.info model.dnd of
                Just { dragIndex } ->
                    if dragIndex /= index then
                        htmlAttribute (Attributes.id colorId) :: List.map htmlAttribute (dndSystem.dropEvents index colorId)

                    else
                        [ htmlAttribute (Attributes.id colorId) ]

                Nothing ->
                    htmlAttribute (Attributes.id colorId) :: List.map htmlAttribute (dndSystem.dragEvents index colorId)
    in
    viewStewedColorWithSurroundings model attributesForDndHandling index color


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


viewStewedColorWithSurroundings : Model -> List (Element.Attribute Msg) -> Int -> Color -> Element Msg
viewStewedColorWithSurroundings model attributesForDndHandling index color =
    let
        colorHsl_ : Result (List DeadEnd) Hsl
        colorHsl_ =
            color
                |> Color.Convert.colorToCssHsl
                |> Parser.run hsl

        backgroundColor : Color
        backgroundColor =
            case model.indexOfHoveredStewedColor of
                Just hoveredIndex ->
                    if index == hoveredIndex then
                        Color.lightGray

                    else
                        Color.white

                Nothing ->
                    Color.white
    in
    case colorHsl_ of
        Ok colorHsl ->
            column
                [ width fill
                , Border.width 1
                , Background.color <| toElmUIColor backgroundColor
                ]
                [ row
                    [ centerX
                    , spacing 10
                    , paddingXY 0 5
                    ]
                    [ text <| Color.Convert.colorToHex color
                    , button
                        [ width <| px 24
                        , height <| px 24
                        , Background.uncropped "assets/clipboard.svg"
                        ]
                        { onPress = Just <| CopyColorCode <| Color.Convert.colorToHex color
                        , label = none
                        }
                    ]
                , el
                    [ centerX
                    , onMouseEnter <| EnterMouseOntoStewedColor index
                    , onMouseLeave <| LeaveMouseFromStewedColor index
                    ]
                    (viewStewedColor attributesForDndHandling color)
                , el
                    [ centerX
                    , width <| px 100
                    , Element.paddingXY 0 5
                    ]
                    (slider
                        [ Background.color <| toElmUIColor Color.lightGray
                        , Border.rounded 10
                        ]
                        { label = labelHidden <| String.fromFloat colorHsl.s
                        , onChange = AdjustSaturation index
                        , min = 0
                        , max = 100
                        , step = Nothing
                        , value = colorHsl.s
                        , thumb =
                            thumb
                                [ Background.color <| toElmUIColor Color.white
                                , Border.width 0
                                , Border.rounded 20
                                , width <| px 20
                                , height <| px 20
                                , Background.uncropped "assets/saturation.svg"
                                ]
                        }
                    )
                , el
                    [ centerX
                    , width <| px 100
                    ]
                    (slider
                        [ Background.color <| toElmUIColor Color.lightGray
                        , Border.rounded 10
                        ]
                        { label = labelHidden <| String.fromFloat colorHsl.l
                        , onChange = AdjustLightness index
                        , min = 0
                        , max = 100
                        , step = Nothing
                        , value = colorHsl.l
                        , thumb =
                            thumb
                                [ Background.color <| toElmUIColor Color.white
                                , Border.width 0
                                , Border.rounded 20
                                , width <| px 20
                                , height <| px 20
                                , Background.uncropped "assets/lightness.svg"
                                ]
                        }
                    )
                ]

        Err _ ->
            column
                [ width fill
                , Border.width 1
                ]
                [ el
                    [ centerX
                    , spacing 10
                    ]
                    (text "#??????")
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


pickDarkColor : Color -> Color
pickDarkColor baseColor =
    let
        colorHsl : Result (List DeadEnd) Hsl
        colorHsl =
            baseColor
                |> colorToCssHsl
                |> Parser.run hsl
    in
    case colorHsl of
        Ok hsl_ ->
            min ((hsl_.l / 100) ^ 2) 0.13
                |> Color.hsl (hsl_.h / 360) (hsl_.s / 100)

        Err _ ->
            Color.black


pickLightColor : Color -> Color
pickLightColor baseColor =
    let
        colorHsl : Result (List DeadEnd) Hsl
        colorHsl =
            baseColor
                |> colorToCssHsl
                |> Parser.run hsl
    in
    case colorHsl of
        Ok hsl_ ->
            max ((hsl_.l / 100) ^ 0.5) 0.97
                |> Color.hsl (hsl_.h / 360) (hsl_.s / 100)

        Err _ ->
            Color.white


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


pickMonochromatic : Color -> List Color
pickMonochromatic baseColor =
    let
        baseColorHsl : Result (List DeadEnd) Hsl
        baseColorHsl =
            baseColor
                |> colorToCssHsl
                |> Parser.run hsl

        makeOverflow : Float -> Float -> Float
        makeOverflow num max =
            if num <= max then
                num

            else
                num - max
    in
    case baseColorHsl of
        Ok colorHsl ->
            List.range 0 4
                |> List.map toFloat
                |> List.map (\index -> Hsl (colorHsl.h / 360) (colorHsl.s / 100) (makeOverflow (colorHsl.l / 100 + index * 0.2) 1))
                |> List.sortBy .l
                |> List.map (\hsl_ -> Color.hsl hsl_.h hsl_.s hsl_.l)

        Err _ ->
            []


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
