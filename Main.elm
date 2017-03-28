module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { str : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "", Cmd.none )



-- Update


type Msg
    = Set String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Set str ->
            ( Model str, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ text model.str
        , input [ onInput Set ]
            []
        ]
