module Plugboard exposing (..)

import EnigmaLetters exposing (EnigmaLetter(..))


-- Model


type alias Pair =
    ( EnigmaLetter, EnigmaLetter )


type alias Plugboard =
    { wireOne : Maybe Pair
    , wireTwo : Maybe Pair
    , wireThree : Maybe Pair
    }


init : Plugboard
init =
    { wireOne = Nothing
    , wireTwo = Nothing
    , wireThree = Nothing
    }



-- `addLink : Letter -> Letter -> Board -> Maybe Board`
-- `getLinks : Board -> List ( Letter, Letter )`
-- `emptyBoard : Board`
-- `removeLink : ( Letter, Letter ) -> Board -> Board`
