module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toUpper, contains)
import Plugboard exposing (Plugboard, Pair, ID, Side(..))
import EnigmaLetters exposing (EnigmaLetter(..))
import Tuple exposing (first)


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
    , nextID : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { str = ""
      , plugboard = Plugboard.init
      , nextID = 0
      }
    , Cmd.none
    )



-- Update


type Msg
    = Set String
    | Connect ID Side String
    | AddWire


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Set str ->
            let
                newStr =
                    Plugboard.encode model.plugboard str
            in
                ( { model | str = newStr }, Cmd.none )

        AddWire ->
            let
                newPlugboard =
                    Plugboard.addWire model.nextID model.plugboard
            in
                ( { model | plugboard = newPlugboard, nextID = model.nextID + 1 }, Cmd.none )

        Connect id side letter ->
            let
                mChar =
                    letter
                        |> toUpper
                        |> String.toList
                        |> List.head

                plugboard =
                    model.plugboard

                newPlugboard =
                    case mChar of
                        Just c ->
                            let
                                eLetter =
                                    EnigmaLetters.fromChar c
                            in
                                Plugboard.pluginContact id side eLetter plugboard

                        Nothing ->
                            Plugboard.plugoutContact id side plugboard
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


letterOption selected_ letter =
    let
        strLetter =
            String.fromChar letter
    in
        option [ value strLetter, selected selected_ ] [ text strLetter ]


selectBox id_ side plugboard =
    let
        alphabetList =
            Plugboard.listOfUnUsedLetters plugboard
                |> EnigmaLetters.toListOfChar

        wirePair =
            Plugboard.getWire id_ plugboard

        addToAlphabetlist mELetter =
            case mELetter of
                Just eLetter ->
                    let
                        char =
                            EnigmaLetters.toChar eLetter
                    in
                        ( Just char
                        , (char :: alphabetList)
                        )

                Nothing ->
                    ( Nothing, alphabetList )

        ( mChar, listWithCurrent ) =
            case wirePair of
                Just ( fst, snd ) ->
                    case side of
                        One ->
                            addToAlphabetlist fst

                        Two ->
                            addToAlphabetlist snd

                Nothing ->
                    ( Nothing, alphabetList )
    in
        div []
            [ select [ onInput <| Connect id_ side ]
                <| List.map (\c -> letterOption (Just c == mChar) c) listWithCurrent
                ++ [ option [ selected <| mChar == Nothing ] [] ]
            ]


wirePair id_ plugboard =
    div [ style [ "display" => "flex" ] ]
        [ text <| "wire " ++ (toString <| id_ + 1)
        , selectBox id_ One plugboard
        , selectBox id_ Two plugboard
        ]


wires plugboard =
    div []
        <| List.map
            (\c ->
                let
                    id_ =
                        first c
                in
                    wirePair id_ plugboard
            )
            plugboard


view : Model -> Html Msg
view model =
    div [ style containerStyle ]
        [ input [ onInput Set ]
            []
        , br [] []
        , text model.str
        , button [ onClick AddWire ] [ text "add wire" ]
        , wires model.plugboard
        ]
