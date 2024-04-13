module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Array (head)
import Data.Maybe

newtype Name = Name {
  aa :: String,
  bb :: Int
}


type X = String

main :: Effect Unit
main = do
  log "hello"

sum :: Int -> Int -> Int
sum a b = a + b

headd :: Array Int -> Int
headd arr = 
  case (head arr) of
    Just x -> x
    Nothing -> 0