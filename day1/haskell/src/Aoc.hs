module Aoc
    ( puzzle1
    , puzzle2
    ) where

import Data.Array (Array, listArray, (!))
import Data.Char (digitToInt)

puzzle1 :: String -> Int 
puzzle1 [] = 0
puzzle1 xs =
  snd $ foldl sum' (last xs, 0) xs
  where
    sum' (ch, acc) y = 
      (y, if ch == y then 
            acc + digitToInt y 
          else 
            acc)

puzzle2 :: String -> Int
puzzle2 [] = 0
puzzle2 xs =
  (*) 2 $ calc mid half arr
  where
    half = take mid xs
    mid = length xs `div` 2
    arr = listArray (0, length xs - 1) xs
    calc mid' ys arr' =
      snd $ foldl sum' (0, 0) ys
      where 
        sum' (ix, acc) y =
          (ix + 1, if y == arr' ! (ix + mid') then
                     acc + digitToInt y
                   else
                     acc)

