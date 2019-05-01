module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Json.Decode
import PlanParsers.Json exposing (..)


type Page
    = InputPage
    | DisplayPage


type Msg
    = NoOp
    | ChangedPlanText String
    | PlanSubmitted


type alias Model =
    { currentPage : Page
    , planText : String
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currentPage = InputPage
      , planText = ""
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPlanText planText ->
            ( { model | planText = planText }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        PlanSubmitted ->
            ( { model | currentPage = DisplayPage }, Cmd.none )



-- VIEW


base : Color
base =
    rgb255 102 99 255


darkest : Color
darkest =
    rgb255 54 52 179


green : Color
green =
    rgb255 86 179 61


lightGreen : Color
lightGreen =
    rgb255 90 204 59


white : Color
white =
    rgb255 255 255 255


defaultBorders =
    { bottom = 0
    , top = 0
    , left = 0
    , right = 0
    }


navbar : Element Msg
navbar =
    row
        [ width fill
        , paddingXY 40 10
        , Border.widthEach { defaultBorders | bottom = 1 }
        , Border.color base
        ]
        [ el [ alignLeft ] <| text "PSQL Expression engine"
        , el [ alignRight ] <| text "Menu"
        ]


inputPage : Model -> Element Msg
inputPage model =
    column
        [ width (fill |> maximum 800)
        , spacingXY 0 10
        , centerX
        , paddingXY 40 10
        ]
        [ Input.multiline
            [ height (px 300)
            , Border.width 1
            , Border.rounded 3
            , Border.color darkest
            , padding 3
            ]
            { label = Input.labelAbove [] <| text "Paste your PSQL Explain output(JSON format):"
            , onChange = ChangedPlanText
            , placeholder = Nothing
            , spellcheck = False
            , text = model.planText
            }
        , Input.button
            [ Background.color green
            , Border.color lightGreen
            , Border.rounded 3
            , Border.widthEach { defaultBorders | bottom = 3 }
            , Font.bold
            , Font.color white
            , paddingXY 20 10
            , alignRight
            , width (px 150)
            , height (px 40)
            ]
            { label = el [ centerX ] <| text "Submit", onPress = Just PlanSubmitted }
        ]


displayPage : Model -> Element Msg
displayPage model =
    let
        tree =
            case Json.Decode.decodeString decodePlanJson model.planText of
                Ok planJson ->
                    text "Decoded!!!"

                Err err ->
                    text <| Json.Decode.errorToString err
    in
    column [ centerX ]
        [ tree
        ]


view : Model -> Browser.Document Msg
view model =
    let
        pageContent =
            case model.currentPage of
                DisplayPage ->
                    displayPage model

                InputPage ->
                    inputPage model
    in
    { title = "Visual Expresser"
    , body =
        [ layout [] <|
            column [ width fill, spacingXY 0 20 ]
                [ navbar
                , pageContent
                ]
        ]
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \sub -> Sub.none
        }
