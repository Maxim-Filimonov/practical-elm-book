module Main exposing (main)

import Attr exposing (..)
import Browser
import Color exposing (..)
import Constants exposing (sampleJSON)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html
import Http
import Json.Decode
import Json.Encode as E
import PlanParsers.Json exposing (..)


type Page
    = InputPage
    | DisplayPage
    | LoginPage


type Msg
    = NoOp
    | ChangedPlanText String
    | PlanSubmitted
    | MouseEnteredPlanNode Plan
    | MouseLeftPlanNode Plan
    | ToggledMenu
    | CreatePlanClicked
    | LoginClicked
    | ChangedPassword String
    | ChangedUserName String
    | LoginSubmitted
    | FinishedLogin (Result Http.Error String)


type alias Model =
    { currentPage : Page
    , planText : String
    , selectedNode : Maybe Plan
    , isModelOpen : Bool
    , userName : String
    , password : String
    , lastError : Maybe String
    , sessionId : Maybe String
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currentPage = InputPage
      , planText = ""
      , selectedNode = Nothing
      , isModelOpen = False
      , lastError = Nothing
      , userName = ""
      , password = ""
      , sessionId = Nothing
      }
      -- ( { currentPage = DisplayPage
      --   , planText = sampleJSON
      --   , selectedNode = Nothing
      --   }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


serverUrl =
    ".netlify/functions/"


login : String -> String -> Cmd Msg
login userName password =
    let
        body : Http.Body
        body =
            Http.jsonBody <|
                E.object
                    [ ( "username", E.string userName )
                    , ( "password", E.string password )
                    ]

        responseDecoder : Json.Decode.Decoder String
        responseDecoder =
            Json.Decode.field "sessionId" Json.Decode.string
    in
    Http.post
        { url = serverUrl ++ "login"
        , body = body
        , expect = Http.expectJson FinishedLogin responseDecoder
        }



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

        ToggledMenu ->
            ( { model | isModelOpen = not model.isModelOpen }, Cmd.none )

        ChangedPassword password ->
            ( { model | password = password }, Cmd.none )

        ChangedUserName userName ->
            ( { model | userName = userName }, Cmd.none )

        CreatePlanClicked ->
            ( { model | currentPage = InputPage, planText = "" }, Cmd.none )

        LoginClicked ->
            ( { model | currentPage = LoginPage, userName = "", password = "" }, Cmd.none )

        LoginSubmitted ->
            ( model, login model.userName model.password )

        FinishedLogin (Ok value) ->
            ( { model | sessionId = Just value, currentPage = InputPage, lastError = Nothing }, Cmd.none )

        FinishedLogin (Err err) ->
            ( { model | lastError = Just <| httpErrorString err }, Cmd.none )


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timeout"



-- VIEW


menuPanel : Model -> Element Msg
menuPanel model =
    let
        items =
            [ el [ pointer, onClick CreatePlanClicked ] <| text "New Plan"
            , el [ pointer, onClick LoginClicked ] <| text "Login"
            ]

        panel =
            column
                [ Background.color white
                , Border.widthEach { defaultBorders | left = 1 }
                , Border.color gray
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 10
                    , color = lightGray
                    }
                , Font.bold
                , Font.color lightGray
                , width fill
                , height fill
                , padding 20
                , spacingXY 0 20
                ]
                items

        overlay =
            el
                [ width <| fillPortion 4
                , height fill
                , onClick ToggledMenu
                ]
                none
    in
    if model.isModelOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none


navbar : Element Msg
navbar =
    row
        [ width fill
        , paddingXY 40 10
        , Border.widthEach { defaultBorders | bottom = 1 }
        , Border.color base
        ]
        [ el [ alignLeft ] <| text "PSQL Expression engine"
        , Input.button
            (grayButton
                ++ [ alignRight
                   , paddingXY 10 5
                   ]
            )
            { onPress = Just ToggledMenu
            , label = el [ centerX ] <| text "Menu"
            }
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
            (greenButton
                ++ [ alignRight
                   , width (px 150)
                   , height (px 40)
                   ]
            )
            { label = el [ centerX ] <| text "Submit"
            , onPress = Just PlanSubmitted
            }
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


loginPage : Model -> Element Msg
loginPage model =
    let
        error =
            case model.lastError of
                Just errorMessage ->
                    el Attr.error <| text errorMessage

                Nothing ->
                    none
    in
    column
        [ paddingXY 0 20
        , spacingXY 0 10
        , width (px 300)
        , centerX
        ]
        [ Input.username Attr.input
            { onChange = ChangedUserName
            , text = model.userName
            , label = Input.labelAbove [] <| text "User name:"
            , placeholder = Nothing
            }
        , Input.currentPassword Attr.input
            { onChange = ChangedPassword
            , text = model.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button Attr.greenButton
            { onPress = Just LoginSubmitted
            , label = el [ centerX ] <| text "Login"
            }
        , error
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

                LoginPage ->
                    loginPage model
    in
    { title = "Visual Expresser"
    , body =
        [ layout
            [ inFront <| menuPanel model
            ]
          <|
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
