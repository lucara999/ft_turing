-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: laraujo <laraujo@student.42lyon.fr>        +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2024/03/07 18:26:08 by laraujo           #+#    #+#             --
--   Updated: 2024/03/08 16:27:02 by laraujo          ###   ########lyon.fr   --
--                                                                            --
-- ************************************************************************** --

module Main where
import System.Environment (getArgs)
import System.Exit
-- import qualified MyLib (someFunc)

data InfiniteTape = InfiniteTape {
  left :: [Char],   -- LazyList
  cursor :: Char,
  right :: [Char]   -- LazyList
}

data TuringMachine = TuringMachine {
  states :: [String],
  alphabet :: [Char],
  init_state :: String,
  final_state :: String,
  transitions :: Transitions
}

type Action = String

data Transition = Transition {
  state :: String,
  read :: Char,
  to_state :: String,
  write :: Char,
  action :: Action
}

type Transitions = [Transition]

initInfiniteTape :: [Char] -> Char -> InfiniteTape
initInfiniteTape input_str blank = InfiniteTape {
  left = repeat blank,
  cursor = head input_str,
  right = tail input_str ++ repeat blank
}

printInfiniteTape :: InfiniteTape -> Int -> IO ()
printInfiniteTape tape size = do
  let resetColor = "\ESC[0m"
  let bold = "\ESC[1m"
  let rev = "\ESC[7m"
  let grey_back = "\ESC[100m"
  let tab = "\t"

  putStr $ tab ++ grey_back ++ reverse (take (size + 2) (left tape))
  putStr $ bold ++ rev ++ [cursor tape] ++ resetColor ++ grey_back
  putStrLn $ take (size * 2) (right tape) ++ resetColor

writeCursor :: InfiniteTape -> Char -> InfiniteTape
writeCursor (InfiniteTape left' _ right') symbole = InfiniteTape left' symbole right'

mvCursorLeft :: InfiniteTape -> InfiniteTape
mvCursorLeft (InfiniteTape [] cursor' right') = InfiniteTape [] cursor' right'
mvCursorLeft (InfiniteTape (l:left') cursor' right') = InfiniteTape left' l (cursor':right')

mvCursorRight :: InfiniteTape -> InfiniteTape
mvCursorRight (InfiniteTape left' cursor' []) = InfiniteTape left' cursor' []
mvCursorRight (InfiniteTape left' cursor' (r:right')) = InfiniteTape (cursor':left') r right'

mvAction :: InfiniteTape -> Action -> InfiniteTape
mvAction tape actionStr
  | actionStr == "LEFT" = mvCursorLeft tape
  | actionStr == "RIGHT" = mvCursorRight tape
  | otherwise = tape

mvRightSearch :: InfiniteTape -> Char -> InfiniteTape
mvRightSearch tape search
  | cursor tape == search = tape
  | otherwise = mvRightSearch (mvCursorRight tape) search

-- findTransition :: Char -> TuringMachine -> Transition
-- findTransition cursor' turing_machine =

-- runTuringMachine :: InfiniteTape -> TuringMachine -> state -> (InfiniteTape, TuringMachine)
-- runTuringMachine tape turing_machine
--   | final_state turing_machine == state = (tape, turing_machine)
--   | otherwise = do 

main :: IO ()
main = do
  putStrLn "###############################################"
  putStrLn "#---------------------------------------------#"
  putStrLn "#---------------- FT_TURING ------------------#"
  putStrLn "#---------------------------------------------#"
  putStrLn "#                                             #"

  args <- getArgs
  if length args < 2 then do
    putStrLn "2 args is required"
    exitSuccess
  else
    putStrLn ""
  let name_file = head args
  let input_str = args !! 1
  let len_input = length input_str
  putStrLn $ "FileName : " ++ name_file
  putStrLn $ "Input_str : " ++ input_str ++ "\n"

  let blank = '.'

  let turing_machine = TuringMachine {
    states = ["impaire", "paire", "HALT"],
    alphabet = ['0', '.'],
    init_state = "impaire",
    final_state = "HALT",
    transitions = [
      Transition "impaire" '0' "paire" '0' "RIGHT",
      Transition "impaire" '.' "HALT" 'y' "RIGHT",
      Transition "paire" '0' "impaire" '0' "RIGHT",
      Transition "paire" '.' "HALT" 'n' "RIGHT"]
  }

  let tape = initInfiniteTape input_str blank
  printInfiniteTape tape len_input


  let tape1 = mvRightSearch tape '='
  printInfiniteTape tape1 len_input

  let tape2 = mvAction tape1 "RIGHT"
  printInfiniteTape tape2 len_input


