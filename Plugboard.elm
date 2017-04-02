module Plugboard exposing (..)

import EnigmaLetters exposing (EnigmaLetter(..))
import List exposing (member, filter, head)
import Tuple exposing (first, second)


-- Model


type alias Pair =
    ( Maybe EnigmaLetter, Maybe EnigmaLetter )


type alias ID =
    Int



-- This is questionable.
-- Used to keep track of what side to update in the Pair Tuple
-- We don't store state for the selectBox
-- So we otherwise dont know which one in the pair should
-- be updated when a new value commes in.
-- If we could figure the currdent value of the selectBox
-- and send that in with the Updated value we could Just
-- match on that
-- ex
-- current (A, B)
-- update A to C
-- after (C, B)
-- this corisponds better to a regular cable since you
-- cannot distingish between the two ends
-- ex
-- (Nothing, Nothing)
-- update Nothing to A
-- take any one of them
-- (A , Nothing)
-- update A to D
-- (D, Nothing)


type Side
    = One
    | Two


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



-- TODO remove maybe


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


listOfUnUsedLetters : Plugboard -> List EnigmaLetter
listOfUnUsedLetters plugboard =
    let
        usedLetters =
            listOfUsedLetters plugboard
    in
        List.filter (\e -> not <| member (Just e) usedLetters) EnigmaLetters.list


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


pluginContact : ID -> Side -> Maybe EnigmaLetter -> Plugboard -> Plugboard
pluginContact id_ side letter plugboard =
    let
        list =
            listOfUsedLetters plugboard

        -- Check that the letter is not already in another connection.
        -- or that we want to set it to Nothing.
        valid =
            (not (member (letter) list)) || letter == Nothing

        updatePlugboard ( wireID, pair ) =
            if wireID == id_ then
                case side of
                    One ->
                        ( wireID, ( letter, second pair ) )

                    Two ->
                        ( wireID, ( first pair, letter ) )
            else
                ( wireID, pair )

        newPlugboard =
            case valid of
                True ->
                    List.map updatePlugboard plugboard

                False ->
                    plugboard
    in
        newPlugboard


plugoutContact : ID -> Side -> Plugboard -> Plugboard
plugoutContact id_ side plugboard =
    pluginContact id_ side Nothing plugboard


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


removeWire : Plugboard -> Plugboard
removeWire =
    List.reverse
        >> List.tail
        >> Maybe.withDefault []
        >> List.reverse



-- List.foldr (\x -> ((::) x)) []
-- let
--     maybe : b -> (List a -> b) -> Maybe (List a) -> b
--     maybe d f =
--         Maybe.withDefault d << Maybe.map f
--
--     res =
--         List.foldr (\x -> maybe [] ((::) x) >> Just) Nothing plug
-- in
--     case res of
--         Just r ->
--             r
--
--         Nothing ->
--             []


getWire : ID -> Plugboard -> Maybe Pair
getWire id_ plugboard =
    let
        filtred =
            List.filter (\( wireId, pair ) -> id_ == wireId) plugboard
                |> List.map (\c -> second c)
    in
        head filtred


tupleMatch : EnigmaLetter -> Pair -> Bool
tupleMatch letter tuple =
    first tuple == Just letter || second tuple == Just letter


findLink : Plugboard -> EnigmaLetter -> Maybe ( ID, Pair )
findLink plugboard letter =
    head <| filter (\p -> tupleMatch letter (getPair p)) plugboard


flip : Pair -> EnigmaLetter -> EnigmaLetter
flip ( fst, snd ) c =
    let
        flip_ side =
            case side of
                Just a ->
                    a

                Nothing ->
                    c
    in
        if fst == Just c then
            flip_ snd
        else if snd == Just c then
            flip_ fst
        else
            c


matchAndFlip : Plugboard -> EnigmaLetter -> EnigmaLetter
matchAndFlip plugboard eLetter =
    case findLink plugboard eLetter of
        Just link ->
            flip (getPair link) eLetter

        Nothing ->
            eLetter


encode : Plugboard -> String -> String
encode plugboard str =
    str
        |> String.toUpper
        |> EnigmaLetters.fromString
        |> List.map (matchAndFlip plugboard)
        |> EnigmaLetters.listToString



-- `getLinks : Board -> List ( Letter, Letter )`
-- `emptyBoard : Board`
-- `removeLink : ( Letter, Letter ) -> Board -> Board`
