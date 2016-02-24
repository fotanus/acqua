module Main where

import Text.Show.Pretty
import L1.Grammar

main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      putStrLn (ppShow ast)
    Left errorMsg ->
      putStrLn errorMsg