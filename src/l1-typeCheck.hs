module Main where

import L1.Grammar
import L1.Type

-- prints the type of a given L1 program

main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      putStrLn (show (typeCheck ast []))
    Left errorMsg ->
      putStrLn errorMsg
