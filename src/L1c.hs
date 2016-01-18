module Main where

import Text.Show.Pretty

import L1.Grammar
import AcquaIR.Language
import AcquaIR.Compile

main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      putStrLn "===Ast==="
      putStrLn (ppShow ast)
      putStrLn "===IR==="
      putStrLn (printProgram (compile ast))
    Left errorMsg ->
      putStrLn errorMsg
