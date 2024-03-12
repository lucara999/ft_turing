-- ************************************************************************** --
--                                                          LE - /            --
--                                                              /             --
--   JSONparsing.hs                                   .::    .:/ .      .::   --
--                                                 +:+:+   +:    +:  +:+:+    --
--   By: fenrir <fenrir@student.le-101.fr>          +:+   +:    +:    +:+     --
--                                                 #+#   #+    #+    #+#      --
--   Created: 2024/03/08 15:03:08 by mrozniec     #+#   ##    ##    #+#       --
--   Updated: 2024/03/12 14:49:04 by fenrir      ###    #+. /#+    ###.fr     --
--                                                         /                  --
--                                                        /                   --
-- ************************************************************************** --
module JSONparsing () where

import Data.Aeson(FromJSON, decode, withObject, (.!), (!))
import Data.ByteString.Lazy (readFile)

data TuringMachine = TuringMachine {
    name :: String,
    alphabet :: [Char],
    blank :: Char,
    states :: [String],
    initial_state :: String,
    finals_state :: [String],
    transitions :: Transitions
} deriving (Show, Generic)

type Action = String

data Transition = Transition {
    state_key :: String,
    read_cur :: Char,
    to_state :: String,
    write_cur :: Char,
    action :: Action
}

type Transitions = [Transition]

parseStringList :: [Value] -> [String]
parseStringList = mapMaybe (\val -> case val of String str -> Just str; _ -> Nothing)

instance FromJSON TuringMachine where
    parseJSON (Object v) = do
        missing <- missingKeys v []
        name <- v .: "name"
        alphabet <- v .: "alphabet" .! parseJSON
        blank <- v .: "blank"
        states <- v .: "states"
        initial_state <- v .: "initial_state"
        finals_state <- v .: "finals_state" .! parseStringList
        transitions <- v .: "transitions" .! parseJSON
        return TuringMachine name alphabet blank states initial_state finals_state transitions
    parseJSON _ = mempty

parseTuring :: IO ()
parseTuring = do
    fileName = "jsonFolder/is02n.json"
    jsonString <- readFile fileName
    maybeData <- decode jsonString
    case maybeData of
        Nothing -> putStrLn "Error parsing JSON file!"
        Just tm -> print tm
