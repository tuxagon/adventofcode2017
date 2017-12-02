module AocDay1
    ( puzzle1
    ) where

import Data.Char (digitToInt)
import Prelude hiding (sum)

puzzle1 :: String -> Int 
puzzle1 []     = 0
puzzle1 (x:xs) =
  let count = snd $ foldl sum (x, 0) xs
  in count + firstLast
  where
    firstLast = 
      if x == l then 
        digitToInt l 
      else 
        0
      where l = last xs
    sum (ch, acc) y = 
      (y, if ch == y then 
            acc + digitToInt y 
          else 
            acc)
