port module Main exposing (main)

import Array exposing (Array)
import Browser
import Color exposing (Color, fromHsla, toHsla, toRgba)
import Color.Convert exposing (colorToHex, hexToColor)
import DnDList
import Element exposing (Attribute, Element, alignRight, alignTop, centerX, centerY, column, el, fill, height, html, htmlAttribute, inFront, layout, maximum, none, padding, paddingEach, paddingXY, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input exposing (button, labelHidden, slider, thumb)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Logo



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


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
    Color.rgb255 192 46 255


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pickedColor = defaultColor
      , stewedColors = pickMonochromatic defaultColor
      , selectedColorScheme = Monochromatic
      , hoveredColorScheme = Nothing
      , indexOfHoveredStewedColor = Nothing
      , dnd = dndSystem.model
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    dndSystem.subscriptions model.dnd


port copyString : String -> Cmd msg



-- UPDATE


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickColor colorHex ->
            ( { model
                | pickedColor =
                    case hexToColor colorHex of
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

                adjustedColor : Result String Color
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

                adjustedColor : Result String Color
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


adjustColor : Model -> HslElement -> Int -> Float -> Result String Color
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
                |> toHsla
                |> (\hsla ->
                        case adjustingElement of
                            Hue ->
                                { hsla | hue = value }

                            Saturation ->
                                { hsla | saturation = value }

                            Lightness ->
                                { hsla | lightness = value }
                   )
                |> fromHsla
                |> Ok

        Nothing ->
            Err "Failed to get an element from an array."


type HslElement
    = Hue
    | Saturation
    | Lightness



-- VIEW


type ColorScheme
    = Dyad
    | DyadPlusDarkAndLight
    | Triad
    | TriadPlusDarkAndLight
    | Analogous
    | Compound
    | CompoundPlusDarkAndLight
    | Square
    | SquarePlusDark
    | SquarePlusLight
    | Rectangle
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

        Analogous ->
            "Analogous"

        Compound ->
            "Compound"

        CompoundPlusDarkAndLight ->
            "Compound + Dark & Light"

        Square ->
            "Square"

        SquarePlusDark ->
            "Square + Dark"

        SquarePlusLight ->
            "Square + Light"

        Rectangle ->
            "Rectangle"

        Pentad ->
            "Pentad"


pickSchemeColors : ColorScheme -> Color -> List Color
pickSchemeColors scheme baseColor =
    case scheme of
        Monochromatic ->
            pickMonochromatic baseColor

        Dyad ->
            pickDyad baseColor

        DyadPlusDarkAndLight ->
            pickDyad baseColor ++ [ pickDarkColor baseColor, pickLightColor baseColor ]

        Triad ->
            pickTriad baseColor

        TriadPlusDarkAndLight ->
            pickTriad baseColor ++ [ pickDarkColor baseColor, pickLightColor baseColor ]

        Analogous ->
            pickAnalogous baseColor

        Compound ->
            pickCompound baseColor

        CompoundPlusDarkAndLight ->
            pickCompound baseColor ++ [ pickDarkColor baseColor, pickLightColor baseColor ]

        Square ->
            pickSquare baseColor

        SquarePlusDark ->
            pickSquare baseColor ++ [ pickDarkColor baseColor ]

        SquarePlusLight ->
            pickSquare baseColor ++ [ pickLightColor baseColor ]

        Rectangle ->
            pickRectangle baseColor

        Pentad ->
            pickPentad baseColor


view : Model -> Html Msg
view model =
    layout
        [ width fill
        , alignTop
        , inFront (viewGhostStewedColor model.dnd model.stewedColors)
        , Font.family
            [ Font.typeface "Latha"
            , Font.sansSerif
            ]
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
    let
        monochromaticColors : List Color
        monochromaticColors =
            pickMonochromatic model.pickedColor
    in
    column
        [ width <| px 320
        , height fill
        ]
        [ row
            [ width fill
            , List.head monochromaticColors
                |> Maybe.withDefault Color.white
                |> toElmUIColor
                |> Background.color
            ]
            [ el
                [ alignRight
                , paddingXY 25 5
                , Font.heavy
                , List.drop 4 monochromaticColors
                    |> List.head
                    |> Maybe.withDefault Color.black
                    |> toElmUIColor
                    |> Font.color
                ]
                (text "Pick Base Color →")
            , html
                (Html.input
                    [ Attributes.type_ "color"
                    , Attributes.value <| colorToHex model.pickedColor
                    , Events.onInput PickColor
                    ]
                    []
                )
            ]
        , el
            [ width fill
            , spacing 10
            , Font.size 15
            , List.drop 4 monochromaticColors
                |> List.head
                |> Maybe.withDefault Color.white
                |> toElmUIColor
                |> Font.color
            , List.drop 1 monochromaticColors
                |> List.head
                |> Maybe.withDefault Color.black
                |> toElmUIColor
                |> Background.color
            ]
            (el
                [ centerX
                , padding 2
                ]
                (text "Color Shemes")
            )
        , viewColorScheme Monochromatic model
        , viewColorScheme Dyad model
        , viewColorScheme DyadPlusDarkAndLight model
        , viewColorScheme Triad model
        , viewColorScheme TriadPlusDarkAndLight model
        , viewColorScheme Analogous model
        , viewColorScheme Compound model
        , viewColorScheme CompoundPlusDarkAndLight model
        , viewColorScheme Square model
        , viewColorScheme SquarePlusDark model
        , viewColorScheme SquarePlusLight model
        , viewColorScheme Rectangle model
        , viewColorScheme Pentad model
        ]


viewColorScheme : ColorScheme -> Model -> Element Msg
viewColorScheme scheme model =
    let
        schemeName : String
        schemeName =
            colorSchemeToString scheme

        schemeColors : List Color
        schemeColors =
            pickSchemeColors scheme model.pickedColor

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
        [ onClick <| SelectScheme scheme schemeColors
        , onMouseLeave <| LeaveMouseFromColorScheme scheme
        , onMouseEnter <| EnterMouseOntoColorScheme scheme
        , spacing 2
        , padding 7
        , Font.size 16
        , width fill
        , Background.color <| toElmUIColor backgroundColor
        ]
        [ text schemeName
        , viewColorSet schemeColors
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
        , height fill
        ]
        [ viewPreview model
        , row
            [ width fill
            , centerY
            ]
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
            , Font.heavy
            , centerX
            , paddingEach
                { top = 20
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
            (html <| Logo.logo 380 logoColor)
        , el
            [ Background.color <| toElmUIColor textBackgroundColor
            , width fill
            ]
            (el
                [ Font.color <| toElmUIColor textColor
                , Font.size 18
                , centerX
                , paddingXY 50 20
                ]
                (paragraph
                    [ width (fill |> maximum 800) ]
                    [ text "Color Stew is a design tool for experiments of color combinations."
                    , text " First pick a base color whatever you like at the top left, then Color Stew automatically generates 11 different color schemes."
                    , text " Just select one of them to check how it looks."
                    , text " Generated color schemes are possibly not so cool."
                    , text " But don't worry. You can adjust each color's saturation and lightness to make it better."
                    , text " You don't like color placements? Just change the order by drag & drop to arrange them."
                    , text " Let's find cool color combinations with Color Scheme."
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

        attributesForDndHandling : List (Attribute Msg)
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


viewGhostStewedColor : DnDList.Model -> List Color -> Element Msg
viewGhostStewedColor dndModel colors =
    let
        maybeDragColor : Maybe Color
        maybeDragColor =
            dndSystem.info dndModel
                |> Maybe.andThen (\{ dragIndex } -> colors |> List.drop dragIndex |> List.head)

        attributesForDndHandling : List (Attribute Msg)
        attributesForDndHandling =
            List.map htmlAttribute (dndSystem.ghostStyles dndModel)
    in
    case maybeDragColor of
        Just color ->
            viewStewedColor attributesForDndHandling color

        Nothing ->
            none


viewStewedColorWithSurroundings : Model -> List (Attribute Msg) -> Int -> Color -> Element Msg
viewStewedColorWithSurroundings model attributesForDndHandling index color =
    let
        colorHsla : Hsla
        colorHsla =
            toHsla color

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
    column
        [ width fill
        , Background.color <| toElmUIColor backgroundColor
        ]
        [ row
            [ centerX
            , spacing 10
            , paddingXY 0 5
            ]
            [ el
                [ Font.size 15 ]
                (text <| colorToHex color)
            , button
                [ width <| px 20
                , height <| px 20
                , Background.uncropped "assets/clipboard.svg"
                ]
                { onPress = Just <| CopyColorCode <| colorToHex color
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
            , paddingXY 0 5
            ]
            (slider
                [ Background.color <| toElmUIColor Color.lightGray
                , Border.rounded 10
                ]
                { label = labelHidden <| String.fromFloat colorHsla.saturation
                , onChange = AdjustSaturation index
                , min = 0
                , max = 1
                , step = Nothing
                , value = colorHsla.saturation
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
                { label = labelHidden <| String.fromFloat colorHsla.lightness
                , onChange = AdjustLightness index
                , min = 0
                , max = 1
                , step = Nothing
                , value = colorHsla.lightness
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


viewStewedColor : List (Attribute Msg) -> Color -> Element Msg
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
        none


pickPolyad : Color -> Int -> List Color
pickPolyad baseColor dimension =
    List.range 0 (dimension - 1)
        |> List.map (pickNthNext baseColor dimension)


pickDarkColor : Color -> Color
pickDarkColor baseColor =
    baseColor
        |> toHsla
        |> (\hsla -> { hsla | lightness = min (hsla.lightness ^ 2) 0.13 })
        |> fromHsla


pickLightColor : Color -> Color
pickLightColor baseColor =
    baseColor
        |> toHsla
        |> (\hsla -> { hsla | lightness = max (hsla.lightness ^ 0.5) 0.97 })
        |> fromHsla


pickDyad : Color -> List Color
pickDyad baseColor =
    pickPolyad baseColor 2


pickTriad : Color -> List Color
pickTriad baseColor =
    pickPolyad baseColor 3


pickAnalogous : Color -> List Color
pickAnalogous baseColor =
    [ baseColor
    , pickNthNext baseColor 12 1
    , pickNthNext baseColor 12 -1
    ]


pickSquare : Color -> List Color
pickSquare baseColor =
    pickPolyad baseColor 4


pickRectangle : Color -> List Color
pickRectangle baseColor =
    [ baseColor
    , pickNthNext baseColor 6 1
    , pickNthNext baseColor 6 3
    , pickNthNext baseColor 6 4
    ]


pickPentad : Color -> List Color
pickPentad baseColor =
    pickPolyad baseColor 5


pickCompound : Color -> List Color
pickCompound color =
    [ color
    , pickNthNext color 12 5
    , pickNthNext color 12 7
    ]


pickMonochromatic : Color -> List Color
pickMonochromatic baseColor =
    let
        makeOverflow : Float -> Float -> Float
        makeOverflow num max =
            if num <= max then
                num

            else
                num - max
    in
    baseColor
        |> toHsla
        |> (\hsla ->
                List.range 0 4
                    |> List.map toFloat
                    |> List.map (\index -> { hsla | lightness = makeOverflow (hsla.lightness + index * 0.2) 1 })
           )
        |> List.sortBy .lightness
        |> List.map fromHsla


pickNthNext : Color -> Int -> Int -> Color
pickNthNext baseColor total n =
    let
        hueDifferenceUnit : Float
        hueDifferenceUnit =
            1 / toFloat total
    in
    baseColor
        |> toHsla
        |> (\hsla ->
                let
                    gainedHue =
                        hsla.hue + toFloat n * hueDifferenceUnit

                    pickedHue =
                        if gainedHue >= 1 then
                            gainedHue - 1

                        else if gainedHue <= -1 then
                            gainedHue + 1

                        else
                            gainedHue
                in
                { hsla | hue = pickedHue }
           )
        |> fromHsla


toElmUIColor : Color -> Element.Color
toElmUIColor color =
    color
        |> toRgba
        |> (\{ red, green, blue, alpha } -> Element.rgba red green blue alpha)


type alias Hsla =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }
