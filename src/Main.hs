-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: laraujo <laraujo@student.42.fr>            +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2024/03/07 18:26:08 by laraujo           #+#    #+#             --
--   Updated: 2024/03/15 18:27:20 by laraujo          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Main where
import System.Environment (getArgs)
import System.Exit
import JSONparsing(parseTuring, TuringMachine(..), Transition(..))
import Data.Map

data InfiniteTape = InfiniteTape {
  left :: [Char],
  cursor :: Char,
  right :: [Char]
}

data TuringMachineLight = TuringMachineLight {
  print_size :: Int,
  len_input :: Int,
  init_state :: String,
  finals_state :: [String],
  transitions_list :: [TransitionUple]
}

data TransitionUple = TransitionUple {
  state_key :: String,
  read_cur :: Char,
  to_state :: String,
  write_cur :: Char,
  action :: String
}

convertMapToList :: Map String [Transition] -> [TransitionUple]
convertMapToList transitionsMap = concatMap expandTransition (Data.Map.toList transitionsMap)
  where
    expandTransition :: (String, [Transition]) -> [TransitionUple]
    expandTransition (state, transitions) = Prelude.map (toTransitionUple state) transitions

    toTransitionUple :: String -> Transition -> TransitionUple
    toTransitionUple stateKey transition = TransitionUple
      { state_key = stateKey
      , read_cur = (head (JSONparsing.read transition))
      , Main.to_state = JSONparsing.to_state transition
      , write_cur = (head (JSONparsing.write transition))
      , Main.action = JSONparsing.action transition
      }

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

 putStr $ tab ++ grey_back ++ reverse (Prelude.take (size + 2) (left tape))
 putStr $ bold ++ rev ++ [cursor tape] ++ resetColor ++ grey_back
 putStrLn $ Prelude.take (size * 2) (right tape) ++ resetColor

printStepTuring :: String -> InfiniteTape -> Int -> String -> IO ()
printStepTuring prefix tape size detail = do
 let resetColor = "\ESC[0m"
 let bold = "\ESC[1m"
 let rev = "\ESC[7m"
 let grey_back = "\ESC[100m"

 putStr $ prefix ++ grey_back ++ reverse (Prelude.take (size + 2) (left tape))
 putStr $ bold ++ rev ++ [cursor tape] ++ resetColor ++ grey_back
 putStrLn $ Prelude.take (size * 2) (right tape) ++ resetColor ++ "\t" ++ detail

printErrorTransition :: TransitionUple -> IO ()
printErrorTransition transition_error = do
 let resetColor = "\ESC[0m"
 let red = "\ESC[1;31m"

 putStrLn $ "\t" ++ red ++ "The machine is blocked transition error:"
 putStrLn $ "\t\t(" ++ state_key transition_error ++ ", " ++ [read_cur transition_error] ++ ") -> (" ++ Main.action transition_error ++ ")" ++ resetColor


writeCursor :: InfiniteTape -> Char -> InfiniteTape
writeCursor (InfiniteTape left' _ right') symbole = InfiniteTape left' symbole right'

mvCursorLeft :: InfiniteTape -> InfiniteTape
mvCursorLeft (InfiniteTape [] cursor' right') = InfiniteTape [] cursor' right'
mvCursorLeft (InfiniteTape (l:left') cursor' right') = InfiniteTape left' l (cursor':right')

mvCursorRight :: InfiniteTape -> InfiniteTape
mvCursorRight (InfiniteTape left' cursor' []) = InfiniteTape left' cursor' []
mvCursorRight (InfiniteTape left' cursor' (r:right')) = InfiniteTape (cursor':left') r right'

mvAction :: InfiniteTape -> String -> InfiniteTape
mvAction tape actionStr
 | actionStr == "LEFT" = mvCursorLeft tape
 | actionStr == "RIGHT" = mvCursorRight tape
 | otherwise = tape

mvRightSearch :: InfiniteTape -> Char -> InfiniteTape
mvRightSearch tape search
 | cursor tape == search = tape
 | otherwise = mvRightSearch (mvCursorRight tape) search

findTransition :: Char -> String -> [TransitionUple] -> String -> TransitionUple
findTransition cursor' state' transitions' final_state =
 case [x | x <- transitions', state_key x == state', read_cur x == cursor'] of
   [] -> TransitionUple state' cursor' final_state cursor' ""
   (x:_) -> x

runTuringMachine :: InfiniteTape -> TuringMachineLight -> String -> IO () --(InfiniteTape, TuringMachine)
runTuringMachine tape turing_machine actual_state
 | actual_state `elem` finals_state turing_machine = do
   putStrLn "Final State reached :"
   printInfiniteTape tape (len_input turing_machine)
 | otherwise = do
   let transition = findTransition (cursor tape) actual_state (transitions_list turing_machine) (head (finals_state turing_machine))
   let tape' = writeCursor tape (write_cur transition)
   let tape'' = mvAction tape' (Main.action transition)

   if Main.action transition `elem` ["LEFT", "RIGHT"] then do
     let str = "(" ++ state_key transition ++ ", " ++ [read_cur transition] ++ ") \t\t‾‾|"
     let str' = "(" ++ Main.to_state transition ++ ", " ++ [write_cur transition] ++ ", " ++ Main.action transition ++ ") \t__|"
     printStepTuring "|‾‾\t" tape (print_size turing_machine) str
     printStepTuring "|__\t" tape' (print_size turing_machine) str'

     runTuringMachine tape'' turing_machine (Main.to_state transition)
   else do
     printStepTuring "\ESC[41m——>\t" tape (print_size turing_machine) "\ESC[41m\t\t\t<——\ESC[0m"
     printErrorTransition transition

printHelp :: IO ()
printHelp = do
 putStrLn $ "usage: ft_turing \ESC[30;3m{-h/--help}\ESC[0m jsonfile input\n"
 putStrLn $ "positional arguments:"
 putStrLn $ "\tjsonfile\t json description of the machine\n"
 putStrLn $ "\tinput\t\t input of the machine\n"
 putStrLn $ "\ESC[30;3moptional arguments:"
 putStrLn $ "\t-h, --help\t  show this help message and exit\ESC[0m"

argsParse :: Int -> [String] -> IO ()
argsParse args_len' args'
 | args_len' == 2 = putStr ""
 | args_len' == 1 = do
   if args' !! 0 == "-h" || args' !! 0 == "--help" then do
     printHelp
     exitSuccess
   else do
     putStrLn "\ESC[1;31m2 args is required:\n\trun: ./ft_turing --help\ESC[0m"
     exitFailure
 | otherwise = do
   putStrLn "\ESC[1;31m2 args is required:\n\trun: ./ft_turing --help\ESC[0m"
   exitFailure

main :: IO ()
main = do
  args <- getArgs
  let args_len = length args
  argsParse args_len args

  let max_print_size = 10
  let name_file = head args
  let input_str = args !! 1
  let len_print = min (length input_str) max_print_size

  putStrLn "#############################################################"
  putStrLn "#-----------------------------------------------------------#"
  putStrLn "#----------------------- FT_TURING -------------------------#"
  putStrLn "#-----------------------------------------------------------#"
  putStrLn "#                                                           #"
  putStrLn $ "FileName : " ++ name_file
  putStrLn $ "Input_str : " ++ input_str ++ "\n"

  turing_machine <- parseTuring input_str name_file
  case (turing_machine) of
    Nothing -> do
      putStrLn "EXIT"
      exitFailure
    Just tm -> do
      putStrLn (show tm)
      -- putStrLn (name tm)

      let turing_machine = TuringMachineLight {
        print_size = len_print,
        len_input = length input_str,
        init_state = initial tm,
        finals_state = finals tm,
        transitions_list = convertMapToList (transitions tm)
      }

      putStrLn "Initial Turing Machine :"
      let tape = initInfiniteTape input_str ((blank tm) !! 0)
      printInfiniteTape tape (length input_str)
      putStrLn "Running Turing Machine :"

      runTuringMachine tape turing_machine (init_state turing_machine)
