module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput)
import String exposing (toUpper, contains)


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
    , plugboard : Pair
    }


type EnigmaLetter
    = A
    | B


type alias Pair =
    ( EnigmaLetter, EnigmaLetter )


init : ( Model, Cmd Msg )
init =
    ( { str = ""
      , plugboard = ( A, B )
      }
    , Cmd.none
    )



-- Update


type Msg
    = Set String


fromEnigmaLetter : EnigmaLetter -> Char
fromEnigmaLetter u =
    case u of
        A ->
            'A'

        B ->
            'B'


fromEnigmaLetters : List EnigmaLetter -> String
fromEnigmaLetters engimaLetterList =
    engimaLetterList
        |> List.map (\c -> fromEnigmaLetter c)
        |> String.fromList


toEnigmaLetter : Char -> Maybe EnigmaLetter
toEnigmaLetter c =
    case c of
        'A' ->
            Just A

        'B' ->
            Just B

        _ ->
            Nothing


toEnigmaLetters : String -> List EnigmaLetter
toEnigmaLetters str =
    str
        |> String.toList
        |> List.filterMap toEnigmaLetter


matchAndFlip : Model -> EnigmaLetter -> EnigmaLetter
matchAndFlip model c =
    if Tuple.first model.plugboard == c then
        Tuple.second model.plugboard
    else
        Tuple.first model.plugboard



-- encode : String -> String


encode matchFnc str =
    str
        |> toUpper
        |> toEnigmaLetters
        |> List.map matchFnc
        |> fromEnigmaLetters


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Set str ->
            let
                matchFnc =
                    matchAndFlip model

                newStr =
                    encode matchFnc str
            in
                ( { model | str = newStr }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput Set ]
            []
        , br [] []
        , text model.str
        ]
