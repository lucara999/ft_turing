-- ************************************************************************** --
--                                                          LE - /            --
--                                                              /             --
--   JSONparsing.hs                                   .::    .:/ .      .::   --
--                                                 +:+:+   +:    +:  +:+:+    --
--   By: mrozniec <mrozniec@student.42lyon.fr>      +:+   +:    +:    +:+     --
--                                                 #+#   #+    #+    #+#      --
--   Created: 2024/03/08 15:03:08 by mrozniec     #+#   ##    ##    #+#       --
--   Updated: 2024/03/13 17:50:28 by mrozniec    ###    #+. /#+    ###.fr     --
--                                                         /                  --
--                                                        /                   --
-- ************************************************************************** --
module JSONparsing () where

import Data.Aeson(FromJSON, decode)
import Data.Map
import Data.ByteString.Lazy (readFile)

data TuringMachine = TuringMachine {
    name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Map String, [Transition]
} deriving (Show, Generic)
instance FromJSON TuringMachine

data Transition = Transition {
    read :: String,
    to_state :: String,
    write :: String,
    action :: String
} deriving (Show, Generic)
instance FromJSON Transition

loadJSON :: String -> IO ( Maybe TuringMachine )
loadJSON fileName = do
    jsonString <- readFile fileName
    return (decode jsonString)

validate :: String -> TuringMachine -> IO ( Maybe String )
validate input tm = do

    --  verification if name is empty
    if (length (name tm)) == 0 then
        Just "empty name inside JSON"

    --  verification if the list of the character of the alphabet is empty
    else if (length (alphabet tm)) == 0 then
        Just "empty alphabet inside JSON"

    --  verification if the character of the alphabet are only 1 char long
    else if (any (\str -> length str /= 1) (alphabet tm)) then
        Just "every character of the alphabet inside the json must be 1 char"

    --  verification if blank is just 1 char long and not empty
    else if (length (blank tm)) /= 1 then
        Just "blank must be 1 char"

    --  verification if blank is part of the alphabet
    else if (blank tm) `notElem` (alphabet tm) then
        Just "blank must be a part of the alphabet"

    --  verification if the list of states is not empty
    else if (length (states tm)) == 0 then
        Just "empty states list inside JSON"

    --  verification if there is an initial state
    else if (length (initial tm)) == 0 then
        Just "empty initial state inside JSON"

    --  verification if the initial state is part of the list of states
    else if (initial tm) `notElem` (states tm) then
        Just "initial state not part of the states inside JSON"

    --  verification if there is finals states
    else if (length (finals tm)) == 0 then
        Just "empty finals states list inside JSON"

    --  verification if the finals states are part of the list of states
    else if (any (\str -> str `notElem` (states tm)) (finals tm)) then
        Just "there is finals states that are not part of the list of states inside the JSON"

    --  verification if there is transitions
    else if (length (transitions tm)) == 0 then
        Just "empty Map of transitions inside JSON"

    --  verification if the transitions states are part of the list of states
    else if (any (\str -> str `notElem` (states tm)) (keys (transitions tm))) then
        Just "a list of transition is linked to an invalid state inside the field transitions of the JSON"

    --  verification if the read inside the transitions are part of the alphabet
    else if (any (\l -> (read l) `notElem` (alphabet tm)) (concat (elems (transitions tm)))) then
        Just "the charactere read inside a transition is not inside the alphabet given inside the JSON"

    --  verification if the write inside the transitions are part of the alphabet
    else if (any (\l -> (write l) `notElem` (alphabet tm)) (concat (elems (transitions tm)))) then
        Just "the charactere write inside a transition is not inside the alphabet given inside the JSON"

    --  verification if the to_state state inside the transitions are part of the list of states
    else if (any (\l -> (to_state l) `notElem` (states tm)) (concat (elems (transitions tm)))) then
        Just "the state of to_state inside a transition is not inside the states listed inside the JSON"

    --  verification if the input is empty
    else if (length input) == 0 then
        Just "input empty"

    --  verification if every char of the input are part of the alphabet
    else if (any (\c -> c `notElem` (alphabet tm)) input) then
        Just "input contain a character not part of the alphabet defined inside the JSON"

    --  verification if there is no blank character inside the input
    else if (blank tm) `notElem` input then
        Just "input contain a blank character"

    --  OK
    else
        Nothing


parseTuring :: String -> String -> IO ( Maybe TuringMachine )
parseTuring input fileName = do
    turingMachine <- loadJSON fileName
    case turingMachine of
        Nothing -> putStrLn "Error parsing JSON file"
        Just tm -> do
            case validate input tm of
                Just err -> putStrLn "Error: " ++ err
                Nothing -> return tm
