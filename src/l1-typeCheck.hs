module Main where

import Text.Show.Pretty
import L1.Grammar
import Compiler.Compile

main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      putStrLn (show (typeCheck ast []))
    Left errorMsg ->
      putStrLn errorMsg
