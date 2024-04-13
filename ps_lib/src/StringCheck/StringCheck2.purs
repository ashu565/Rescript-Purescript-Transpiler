module StringCheck.StringCheck2 where

import Prelude
import Data.Maybe
import Data.Array

isThereEvenNumber2 :: Array Int -> Boolean
isThereEvenNumber2 numbers = 
  case uncons numbers of
    Just { head: x, tail: xs } -> if x `mod` 2 == 0 then true else isThereEvenNumber2 xs
    Nothing -> false