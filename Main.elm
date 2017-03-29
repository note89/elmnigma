module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toUpper, contains)
import Plugboard exposing (Plugboard, Pair)
import EnigmaLetters exposing (EnigmaLetter(..))


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
    , plugboard : Plugboard
    }


init : ( Model, Cmd Msg )
init =
    ( { str = ""
      , plugboard = Plugboard.init
      }
    , Cmd.none
    )



-- Update


type Msg
    = Set String
    | Connect EnigmaLetter EnigmaLetter


fromEnigmaLetter : EnigmaLetter -> Char
fromEnigmaLetter u =
    case u of
        A ->
            'A'

        B ->
            'B'

        C ->
            'C'

        D ->
            'D'


fromEnigmaLetters : List EnigmaLetter -> String
fromEnigmaLetters engimaLetterList =
    engimaLetterList
        |> List.map fromEnigmaLetter
        |> String.fromList


toEnigmaLetter : Char -> Maybe EnigmaLetter
toEnigmaLetter c =
    case c of
        'A' ->
            Just A

        'B' ->
            Just B

        'C' ->
            Just C

        'D' ->
            Just D

        _ ->
            Nothing


toEnigmaLetters : String -> List EnigmaLetter
toEnigmaLetters str =
    str
        |> String.toList
        |> List.filterMap toEnigmaLetter


flipOrPass : Maybe Pair -> EnigmaLetter -> EnigmaLetter
flipOrPass wire c =
    case wire of
        Just ( fst, snd ) ->
            if fst == c then
                snd
            else if snd == c then
                fst
            else
                c

        Nothing ->
            c


matchAndFlip : Plugboard -> EnigmaLetter -> EnigmaLetter
matchAndFlip plugboard c =
    let
        { wireOne, wireTwo, wireThree } =
            plugboard
    in
        c
            |> flipOrPass wireOne
            |> flipOrPass wireTwo
            |> flipOrPass wireThree


encode : (EnigmaLetter -> EnigmaLetter) -> String -> String
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
                    matchAndFlip model.plugboard

                newStr =
                    encode matchFnc str
            in
                ( { model | str = newStr }, Cmd.none )

        Connect a b ->
            let
                plugboard =
                    model.plugboard

                newPlugboard =
                    { plugboard | wireOne = Just ( a, b ) }
            in
                ( { model | plugboard = newPlugboard }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


(=>) =
    (,)


containerStyle =
    [ "height" => "100%"
    , "display" => "flex"
    , "flex" => "1"
    , "justify-content" => "center"
    , "align-items" => "center"
    , "flex-direction" => "column"
    , "flex-grow" => "1"
    ]


view : Model -> Html Msg
view model =
    div [ style containerStyle ]
        [ input [ onInput Set ]
            []
        , br [] []
        , text model.str
        , div [ onClick <| Connect A B ] [ text "connect A and B" ]
        ]
