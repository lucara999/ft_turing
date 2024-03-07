-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: laraujo <laraujo@student.42.fr>            +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2024/03/07 18:26:08 by laraujo           #+#    #+#             --
--   Updated: 2024/03/07 18:26:13 by laraujo          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Main where
import System.Environment (getArgs)
import System.Exit

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
