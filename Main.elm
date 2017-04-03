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
    | RemoveWire


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

        RemoveWire ->
            let
                newPlugboard =
                    Plugboard.removeWire model.plugboard
            in
                ( { model | plugboard = newPlugboard, nextID = model.nextID - 1 }, Cmd.none )

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


letterOption letter =
    let
        strLetter =
            String.fromChar letter
    in
        option [ value strLetter ] [ text strLetter ]


selectBox side ( id_, ( fst, snd ) ) plugboard =
    let
        mELetter =
            case side of
                One ->
                    fst

                Two ->
                    snd

        alphabetList =
            plugboard
                |> Plugboard.listOfUnUsedLettersPlusCurrent mELetter
                |> EnigmaLetters.toListOfChar
    in
        div []
            [ select [ onInput <| Connect id_ side ]
                <| List.map
                    (\c ->
                        letterOption c
                    )
                    alphabetList
                ++ [ option [ selected <| mELetter == Nothing ] [] ]
            ]


wirePair entry plugboard =
    div [ style [ "display" => "flex" ] ]
        [ text <| "wire " ++ (toString <| (first entry) + 1)
        , selectBox One entry plugboard
        , selectBox Two entry plugboard
        ]


wires plugboard =
    div []
        <| List.map
            (\entry ->
                wirePair entry plugboard
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
        , button [ onClick RemoveWire ] [ text "remove wire" ]
        , wires model.plugboard
        ]
