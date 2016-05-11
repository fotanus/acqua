module Main where

import Text.Show.Pretty
import L1.Grammar
import UL1.FromL1

-- prints the AST of a L1 program after being parsed
main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      putStrLn (ppShow (fromL1 ast))
    Left errorMsg ->
      putStrLn errorMsg
