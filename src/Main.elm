module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Border as Border
import Html


type Page
    = InputPage


type Msg
    = NoOp


type alias Model =
    { currentPage : Page
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currentPage = InputPage
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
        _ ->
            ( model, Cmd.none )



-- VIEW


base : Color
base =
    rgb255 102 99 255


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


view : Model -> Browser.Document Msg
view _ =
    { title = "Visual Expresser"
    , body =
        [ layout [] <|
            column [ width fill, spacingXY 0 20 ]
                [ navbar
                , el [ centerX ] <| text "PSQL Expressions"
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
