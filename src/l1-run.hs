module Main where

import Text.Show.Pretty
import L1.Grammar
import L1.Reduce

-- Reduces a L1 program - not working
main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      putStrLn (ppShow (run ast))
    Left errorMsg ->
      putStrLn errorMsg
