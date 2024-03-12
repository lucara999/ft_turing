-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: laraujo <laraujo@student.42.fr>            +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2024/03/07 18:26:08 by laraujo           #+#    #+#             --
--   Updated: 2024/03/12 14:30:17 by laraujo          ###   ########.fr       --
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
  print_size :: Int,
  len_input :: Int,
  states :: [String],
  alphabet :: [Char],
  init_state :: String,
  finals_state :: [String],
  transitions :: Transitions
}

type Action = String

data Transition = Transition {
  state_key :: String,
  read_cur :: Char,
  to_state :: String,
  write_cur :: Char,
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

printStepTuring :: String -> InfiniteTape -> Int -> String -> IO ()
printStepTuring prefix tape size detail = do
  let resetColor = "\ESC[0m"
  let bold = "\ESC[1m"
  let rev = "\ESC[7m"
  let grey_back = "\ESC[100m"

  putStr $ prefix ++ grey_back ++ reverse (take (size + 2) (left tape))
  putStr $ bold ++ rev ++ [cursor tape] ++ resetColor ++ grey_back
  putStrLn $ take (size * 2) (right tape) ++ resetColor ++ "\t" ++ detail

printErrorTransition :: Transition -> IO ()
printErrorTransition transition_error = do
  let resetColor = "\ESC[0m"
  let red = "\ESC[1;31m"

  putStrLn $ "\t" ++ red ++ "The machine is blocked transition error:"
  putStrLn $ "\t\t(" ++ state_key transition_error ++ ", " ++ [read_cur transition_error] ++ ") -> (" ++ action transition_error ++ ")" ++ resetColor

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

findTransition :: Char -> String -> [Transition] -> String -> Transition
findTransition cursor' state' transitions' final_state =
  case [x | x <- transitions', state_key x == state', read_cur x == cursor'] of
    [] -> Transition state' cursor' final_state cursor' ""
    (x:_) -> x

runTuringMachine :: InfiniteTape -> TuringMachine -> String -> IO () --(InfiniteTape, TuringMachine)
runTuringMachine tape turing_machine actual_state
  | actual_state `elem` finals_state turing_machine = do
    putStrLn "Final State reached :"
    printInfiniteTape tape (len_input turing_machine)
  | otherwise = do
    let transition = findTransition (cursor tape) actual_state (transitions turing_machine) (head (finals_state turing_machine))
    let tape' = writeCursor tape (write_cur transition)
    let tape'' = mvAction tape' (action transition)

    if action transition `elem` ["LEFT", "RIGHT"] then do
      let str = "(" ++ state_key transition ++ ", " ++ [read_cur transition] ++ ") \t\t‾‾|"
      let str' = "(" ++ to_state transition ++ ", " ++ [write_cur transition] ++ ", " ++ action transition ++ ") \t__|"
      printStepTuring "|‾‾\t" tape (print_size turing_machine) str
      printStepTuring "|__\t" tape' (print_size turing_machine) str'

      runTuringMachine tape'' turing_machine (to_state transition)
    else do
      printStepTuring "\ESC[41m-->\t" tape (print_size turing_machine) "\ESC[41m\t\t\t<--\ESC[0m"
      printErrorTransition transition

main :: IO ()
main = do
  putStrLn "#############################################################"
  putStrLn "#-----------------------------------------------------------#"
  putStrLn "#----------------------- FT_TURING -------------------------#"
  putStrLn "#-----------------------------------------------------------#"
  putStrLn "#                                                           #"

  args <- getArgs
  if length args < 2 then do
    putStrLn "2 args is required"
    exitSuccess
  else
    putStrLn ""
  let max_print_size = 10
  let name_file = head args
  let input_str = args !! 1
  let len_print = min (length input_str) max_print_size
  putStrLn $ "FileName : " ++ name_file
  putStrLn $ "Input_str : " ++ input_str ++ "\n"

  let blank = '.'

  let turing_machine = TuringMachine {
    print_size = len_print,
    len_input = length input_str,
    states = ["impaire", "paire", "HALT", "STOP"],
    alphabet = ['0', '.'],
    init_state = "impaire",
    finals_state = ["HALT", "STOP"],
    transitions = [
      Transition "impaire" '0' "paire" '0' "RIGHT",
      Transition "impaire" '.' "HALT" 'y' "RIGHT",
      Transition "paire" '0' "impaire" '0' "RIGHT",
      Transition "paire" '.' "HALT" 'n' "RIGHT"]
  }

  putStrLn "Initial Turing Machine :"
  let tape = initInfiniteTape input_str blank
  printInfiniteTape tape (length input_str)
  putStrLn "Running Turing Machine :"

  runTuringMachine tape turing_machine (init_state turing_machine)
  -- printInfiniteTape tape' len_input

  -- let tape1 = mvRightSearch tape '='
  -- printInfiniteTape tape1 len_input

  -- let tape2 = mvAction tape1 "RIGHT"
  -- printInfiniteTape tape2 len_input


