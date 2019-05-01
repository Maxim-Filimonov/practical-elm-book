module Main exposing (main)

import Browser
import Constants exposing (sampleJSON)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
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
    | MouseEnteredPlanNode Plan
    | MouseLeftPlanNode Plan


type alias Model =
    { currentPage : Page
    , planText : String
    , selectedNode : Maybe Plan
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    -- ( { currentPage = InputPage
    --   , planText = ""
    --   , selectedNode = Nothing
    --   }
    ( { currentPage = DisplayPage
      , planText = sampleJSON
      , selectedNode = Nothing
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

        MouseEnteredPlanNode plan ->
            ( { model | selectedNode = Just plan }, Cmd.none )

        MouseLeftPlanNode plan ->
            ( { model | selectedNode = Nothing }, Cmd.none )



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


highlight : Color
highlight =
    rgb255 255 170 125


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


planNodeTree : Plan -> List (Element Msg)
planNodeTree plan =
    let
        viewNodeType nodeType =
            el [ Font.bold ] <| text nodeType

        viewNode node nodeDetails =
            [ el
                [ Border.widthEach { defaultBorders | bottom = 1 }
                , Border.color lightGreen
                , mouseOver [ Background.color highlight ]
                , padding 4
                , onMouseEnter <| MouseEnteredPlanNode plan
                , onMouseLeave <| MouseLeftPlanNode plan
                ]
              <|
                paragraph []
                    (viewNodeType node.common.nodeType :: nodeDetails)
            , childNodeTree node.common.plans
            ]
    in
    case plan of
        PCTe cteNode ->
            viewNode cteNode
                [ text " on "
                , el [ Font.italic ] <| text cteNode.cteName
                , text <| " (" ++ cteNode.alias_ ++ ") "
                ]

        PGeneric genericNode ->
            viewNode { common = genericNode }
                []

        PResult resultNode ->
            viewNode resultNode []

        PSort sortNode ->
            viewNode sortNode
                [ text " on "
                , el [ Font.italic ] <|
                    text <|
                        String.join ", " sortNode.sortKey
                ]

        PSeqScan seqScanNode ->
            viewNode seqScanNode
                [ text " on "
                , el [ Font.italic ] <| text seqScanNode.relationName
                , text <| " (" ++ seqScanNode.alias_ ++ ") "
                ]


childNodeTree : Plans -> Element Msg
childNodeTree (Plans plans) =
    column [ paddingEach { left = 20, bottom = 0, top = 0, right = 0 } ] <|
        List.concatMap planNodeTree plans


viewAttributes : Plan -> List (Element Msg)
viewAttributes plan =
    let
        viewAttr name value =
            wrappedRow [ width fill ]
                [ el [ width (px 200) ] <| text name
                , paragraph [ width fill, Font.bold ] [ text value ]
                ]

        header name =
            el
                [ paddingEach
                    { top = 10
                    , bottom = 5
                    , left = 10
                    , right = 0
                    }
                ]
            <|
                el
                    [ Font.bold
                    , Border.widthEach { defaultBorders | bottom = 1 }
                    , Border.color lightGreen
                    ]
                <|
                    text name

        commonAttrs common =
            [ viewAttr "Startup Cost" <| String.fromFloat common.startupCost
            , viewAttr "Total Cost" <| String.fromFloat common.totalCost
            , viewAttr "Schema" <| common.schema
            ]
    in
    case plan of
        PCTe node ->
            commonAttrs node.common

        PGeneric node ->
            commonAttrs node

        PResult node ->
            commonAttrs node.common

        PSeqScan node ->
            commonAttrs node.common
                ++ [ header "Filter"
                   , viewAttr "Filter" node.filter
                   , viewAttr "Width" <| String.fromInt node.rowsRemovedByFilter
                   ]

        PSort node ->
            commonAttrs node.common
                ++ [ header "Sort"
                   , viewAttr "Sort Key" <| String.join ", " node.sortKey
                   , viewAttr "Sort Method" node.sortMethod
                   , viewAttr "Sort Space Type" node.sortSpaceType
                   , viewAttr "Sort Space Used" <| String.fromInt node.sortSpaceUsed
                   ]


displayPage : Model -> Element Msg
displayPage model =
    let
        tree =
            case Json.Decode.decodeString decodePlanJson model.planText of
                Ok planJson ->
                    planNodeTree planJson.plan

                Err err ->
                    [ text <| Json.Decode.errorToString err ]

        attributes =
            case model.selectedNode of
                Nothing ->
                    [ text "" ]

                Just plan ->
                    viewAttributes plan
    in
    row
        [ width fill
        , paddingEach
            { top = 20
            , left = 0
            , bottom = 0
            , right = 0
            }
        ]
        [ column
            [ width (fillPortion 7)
            , height fill
            , alignTop
            ]
            tree
        , column
            [ width (fillPortion 3 |> maximum 500)
            , height fill
            , alignTop
            , padding 5
            , Border.color base
            , Border.widthEach { defaultBorders | left = 1 }
            ]
          <|
            attributes
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
