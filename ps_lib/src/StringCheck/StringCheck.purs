module StringCheck.StringCheck where

import Prelude
import Data.Maybe
import Data.Array

isThereEvenNumber :: Array Int -> Boolean
isThereEvenNumber numbers = 
  case uncons numbers of
    Just { head: x, tail: xs } -> if x `mod` 2 == 0 then true else isThereEvenNumber xs
    Nothing -> false