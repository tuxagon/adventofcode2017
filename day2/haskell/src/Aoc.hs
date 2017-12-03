module Aoc
    ( puzzle1
    , puzzle2
    ) where

import Data.Maybe (fromMaybe)

puzzle1 :: [[Int]] -> Int
puzzle1 []    = 0
puzzle1 sheet =
  sum $ map diff sheet
  where diff row = maximum row - minimum row

puzzle2 :: [[Int]] -> Int
puzzle2 []    = 0
puzzle2 sheet =
  sum $ map evenlyDivide sheet
  where
    evenlyDivide row = 
      foldl (\x acc -> if acc == 0 then x else acc) 0 $ map (divide row) row
    divide row n = fromMaybe 0 $ case result of
      [] -> Nothing
      x -> Just $ last x `div` n 
      where greater = filter (> n) row
            result = filter (\x -> x `mod` n == 0) greater
