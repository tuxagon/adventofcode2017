module Aoc
    ( d1puzzle1
    , d1puzzle2
    , d2puzzle1
    , d2puzzle2
    ) where

import Data.Array (Array, listArray, (!))
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

d1puzzle1 :: String -> Int 
d1puzzle1 [] = 0
d1puzzle1 xs =
  snd $ foldl sum' (last xs, 0) xs
  where
    sum' (ch, acc) y = 
      (y, if ch == y then 
            acc + digitToInt y 
          else 
            acc)

d1puzzle2 :: String -> Int
d1puzzle2 [] = 0
d1puzzle2 xs =
  (*) 2 $ calc mid half arr
  where
    half = take mid xs
    mid = length xs `div` 2
    arr = listArray (0, length xs - 1) xs
    calc mid' ys arr' =
      snd $ foldl sum' (0, 0) ys
      where 
        sum' :: (Int, Int) -> Char -> (Int, Int)
        sum' (ix, acc) y =
          (ix + 1, if y == arr' ! (ix + mid') then
                     acc + digitToInt y
                   else
                     acc)

d2puzzle1 :: [[Int]] -> Int
d2puzzle1 []    = 0
d2puzzle1 sheet =
  sum $ map diff sheet
  where diff row = maximum row - minimum row

d2puzzle2 :: [[Int]] -> Int
d2puzzle2 []   = 0
d2puzzle2 sheet =
  sum $ map evenlyDivide sheet
  where
    evenlyDivide row = 
      foldl (\x acc -> if acc == 0 then x else acc) 0 $ map (divide row) row
    divide row n = fromMaybe 0 $ case result of
      [] -> Nothing
      x -> Just $ last x `div` n 
      where greater = filter (> n) row
            result = filter (\x -> x `mod` n == 0) greater
