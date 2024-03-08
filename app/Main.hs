-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: laraujo <laraujo@student.42.fr>            +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2024/03/07 18:26:08 by laraujo           #+#    #+#             --
--   Updated: 2024/03/08 10:17:51 by laraujo          ###   ########.fr       --
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

initInfiniteTape :: Char -> InfiniteTape
initInfiniteTape blank = InfiniteTape {
  left = repeat blank,
  cursor = blank,
  right = repeat blank
}

printInfiniteTape :: InfiniteTape -> Int -> IO ()
printInfiniteTape tape size = do
  let resetColor = "\ESC[0m"
  let bold = "\ESC[1m"
  let rev = "\ESC[7m"
  let grey_back = "\ESC[100m"

  putStr $ grey_back ++ (reverse (take (size) (left tape)))
  putStr $ bold ++ rev ++ [cursor tape] ++ resetColor ++ grey_back
  putStrLn $ (take (size * 2) (right tape)) ++ resetColor

writeCursor :: InfiniteTape -> Char -> InfiniteTape
writeCursor (InfiniteTape left _ right) symbole = InfiniteTape left symbole right

mvCursorLeft :: InfiniteTape -> InfiniteTape
mvCursorLeft (InfiniteTape (l:left) cursor right) = InfiniteTape left l (cursor:right)

mvCursorRight :: InfiniteTape -> InfiniteTape
mvCursorRight (InfiniteTape left cursor (r:right)) = InfiniteTape (cursor:left) r right


main :: IO ()
main = do
  putStrLn "-------------------------------------------------"
  putStrLn "------------------ FT_TURING --------------------"
  putStrLn "-------------------------------------------------"

  args <- getArgs
  if length args < 2 then do
    putStrLn "2 args is required"
    exitSuccess
  else
    putStrLn "OK"
  let name_file = args !! 0
  let input_str = args !! 1
  let len_input = length input_str
  putStrLn $ "FileName : " ++ name_file
  putStrLn $ "Input_str : " ++ input_str

  let blank = '.'

  let tape = initInfiniteTape blank
  printInfiniteTape tape len_input

  let tape1 = writeCursor tape '1'
  printInfiniteTape tape1 len_input
  let tape2 = mvCursorRight tape1
  printInfiniteTape tape2 len_input

  let tape3 = writeCursor tape2 '0'
  printInfiniteTape tape3 len_input
  let tape4 = mvCursorRight tape3
  printInfiniteTape tape4 len_input

  let tape5 = writeCursor tape4 '0'
  printInfiniteTape tape5 len_input
  let tape6 = mvCursorRight tape5
  printInfiniteTape tape6 len_input

  let tape7 = writeCursor tape6 '0'
  printInfiniteTape tape7 len_input
  let tape8 = mvCursorRight tape7
  printInfiniteTape tape8 len_input

