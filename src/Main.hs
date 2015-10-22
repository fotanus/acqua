module Main where

import Text.Show.Pretty

import L1.Language
import L1.Grammar

main = do
  s <- getContents
  let ast = parse s
  case ast of
    Right ast ->
      putStrLn (ppShow ast)
    Left error ->
      putStrLn error
