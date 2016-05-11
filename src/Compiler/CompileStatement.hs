module Compiler.CompileStatement where

import AcquaIR.Language as IR

data Statement
  = SC IR.Command
  | ST IR.Terminator
  | SL Label
  deriving (Eq,Ord,Show,Read)

statementsToProgram :: [Statement] -> IR.Program
statementsToProgram st = map toBasicBlock (splitBasicBlocks st)

toBasicBlock :: [Statement] -> BasicBlock
toBasicBlock [] = error "Wrong argument to toBasicBlock"
toBasicBlock (x:xs) = IR.BB l s cs t
  where
    SL l = x
    ST t = last xs
    cs = map (\(SC c) -> c) (init xs)
    s = 0

splitBasicBlocks :: [Statement] -> [[Statement]]
splitBasicBlocks xs = split xs
  where split [] = []
        split xs' = case break isTerminator xs' of
          (chunk,[])         -> chunk : []
          (chunk,(x:rest))   -> (chunk ++ [x]) : split rest
        isTerminator :: Statement -> Bool
        isTerminator (ST _) = True
        isTerminator _ = False


