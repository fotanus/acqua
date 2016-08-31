module Simulator.List where

import Simulator.Value

data List = List {
  size :: Int,
  params :: [Value]
} deriving (Show,Eq)

emptyList :: List
emptyList = List 0 []

listSetPos :: [Value] -> Int -> Value -> [Value]
listSetPos vs n v = take n vs ++ [v] ++ drop (n + 1) vs
