module Plugboard exposing (..)

import EnigmaLetters exposing (EnigmaLetter(..))
import List exposing (member, filter, head)
import Tuple exposing (first, second)


-- Model


type alias Pair =
    ( Maybe EnigmaLetter, Maybe EnigmaLetter )


type alias ID =
    Int


initWire : Pair
initWire =
    ( Nothing, Nothing )


type alias Plugboard =
    List ( ID, Pair )


init : Plugboard
init =
    []


getPair : ( ID, Pair ) -> Pair
getPair entry =
    second entry


listOfUsedLetters : Plugboard -> List (Maybe EnigmaLetter)
listOfUsedLetters plugboard =
    List.concatMap
        (\t ->
            let
                p =
                    getPair t
            in
                [ first p, second p ]
        )
        plugboard


addLink : EnigmaLetter -> EnigmaLetter -> Plugboard -> Plugboard
addLink l1 l2 plugboard =
    let
        list =
            listOfUsedLetters plugboard

        valid =
            ((member (Just l1) list) || (member (Just l2) list))
                && (List.length plugboard < 10)

        newPlugboard =
            case valid of
                True ->
                    plugboard

                False ->
                    plugboard ++ [ ( 0, ( Just l1, Just l2 ) ) ]
    in
        newPlugboard


addWire : ID -> Plugboard -> Plugboard
addWire id plugboard =
    let
        valid =
            List.length plugboard < 10

        newPlugboard =
            case valid of
                True ->
                    plugboard ++ [ ( id, initWire ) ]

                False ->
                    plugboard
    in
        newPlugboard


tupleMatch : EnigmaLetter -> Pair -> Bool
tupleMatch letter tuple =
    first tuple == Just letter || second tuple == Just letter


findLink : Plugboard -> EnigmaLetter -> Maybe ( ID, Pair )
findLink plugboard letter =
    head <| filter (\p -> tupleMatch letter (getPair p)) plugboard



-- `getLinks : Board -> List ( Letter, Letter )`
-- `emptyBoard : Board`
-- `removeLink : ( Letter, Letter ) -> Board -> Board`
