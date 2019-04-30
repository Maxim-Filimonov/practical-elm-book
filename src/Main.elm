module Main exposing (main)

import Browser
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


view : Model -> Browser.Document Msg
view _ =
    { title = "Visual Expresser", body = [] }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \sub -> Sub.none
        }
