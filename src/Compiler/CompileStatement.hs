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


printStatements :: [Statement] -> String
printStatements [] = ""
printStatements (x:xs) = printStatement(x) ++ printStatements(xs)

printStatement :: Statement-> String
printStatement (SC (EnvNew i n)) = ident ++ i ++ " = EnvNew " ++ (show n) ++ "\n"
printStatement (SC (EnvAddI i name n)) = ident ++ "EnvAdd " ++ i ++ " " ++ name ++ " " ++ (show n) ++ "\n"
printStatement (SC (EnvAddL i name l)) = ident ++ "EnvAdd " ++ i ++ " " ++ name ++ " " ++ l ++ "\n"
printStatement (SC (Call name1 name2 i)) = ident ++ name1 ++ " = Call " ++ name2 ++ " " ++ i ++ "\n"
printStatement (SC (IR.Op name1 op name2)) = ident ++ "resp = " ++ name1 ++ " " ++ (printOpCode op) ++ " " ++ name2 ++ "\n"
printStatement (SC (AssignI name n)) = ident ++ name ++ " = " ++ (show n) ++ "\n"
printStatement (SC (AssignL name l)) = ident ++ name ++ " = " ++ l ++ "\n"
printStatement (SC (AssignV name1 name2)) = ident ++ name1 ++ " = " ++ name2 ++ "\n"
printStatement (SC (Wait)) = ident ++ "Wait" ++ "\n"
printStatement (ST (Goto l)) = ident ++ "goto " ++ l ++ "\n"
printStatement (ST (Return name)) = ident ++ "return " ++ name ++ "\n"
printStatement (ST (IR.If name l)) = ident ++ "if " ++ name ++ " goto " ++ l ++ "\n"
printStatement (ST Empty) = ident ++ "empty\n"

printStatement (SL name) = name ++ ":\n"
