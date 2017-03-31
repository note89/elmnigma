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



-- Can we make sure that Enimgaletter is part
-- of the Pair ?


flip : Pair -> EnigmaLetter -> EnigmaLetter
flip ( fst, snd ) c =
    if fst == Just c then
        case snd of
            Just a ->
                a

            Nothing ->
                c
    else if snd == Just c then
        case fst of
            Just a ->
                a

            Nothing ->
                c
    else
        c


matchAndFlip : Plugboard -> EnigmaLetter -> EnigmaLetter
matchAndFlip plugboard c =
    case Plugboard.findLink plugboard c of
        Just link ->
            flip (Plugboard.getPair link) c

        Nothing ->
            c


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

        AddWire ->
            let
                newPlugboard =
                    Plugboard.addWire model.nextID model.plugboard
            in
                ( { model | plugboard = newPlugboard, nextID = model.nextID + 1 }, Cmd.none )

        Connect id side letter ->
            let
                char =
                    letter
                        |> toUpper
                        |> String.toList
                        |> List.head

                plugboard =
                    model.plugboard

                newPlugboard =
                    case char of
                        Just c ->
                            let
                                eLetter =
                                    toEnigmaLetter c
                            in
                                Plugboard.pluginContact id side eLetter plugboard

                        Nothing ->
                            plugboard
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


alphabetList : List String
alphabetList =
    [ "A"
    , "B"
    , "C"
    , "D"
    , "E"
    , "F"
    , "G"
    , "H"
    , "I"
    , "J"
    , "K"
    , "L"
    , "M"
    , "N"
    , "O"
    , "P"
    , "Q"
    , "R"
    , "S"
    , "T"
    , "U"
    , "V"
    , "W"
    , "X"
    , "Y"
    , "Z"
    ]


letterOption letter =
    option [ value letter ] [ text letter ]


selectBox id_ side =
    div []
        [ select [ onInput <| Connect id_ side ]
            <| List.map letterOption alphabetList
            ++ [ option [ selected True ] [] ]
        ]


wirePair id_ =
    div [ style [ "display" => "flex" ] ]
        [ text <| "wire " ++ (toString <| id_ + 1)
        , selectBox id_ One
        , selectBox id_ Two
        ]


wires plugboard =
    div []
        <| List.map
            (\c ->
                let
                    text =
                        "wire " ++ (toString <| (first c) + 1)

                    id_ =
                        first c
                in
                    wirePair id_
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
