module Aoc
    ( d1puzzle1
    , d1puzzle2
    , d2puzzle1
    ) where

import Data.Array (Array, listArray, (!))
import Data.Char (digitToInt)

d1puzzle1 :: String -> Int 
d1puzzle1 [] = 0
d1puzzle1 xs =
  snd $ foldl sum' (end, 0) xs
  where
    end = last xs
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
