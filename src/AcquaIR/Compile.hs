module AcquaIR.Compile where

import L1.Language as L1
import AcquaIR.Language as IR
import Control.Monad.State

data Statement
  = SC IR.Command
  | ST IR.Terminator
  | SL Label
  deriving (Eq,Ord,Show,Read)

data States = States {
  thenLabelNum :: Int,
  backLabelNum :: Int,
  dummyLabelNum :: Int,
  identNum :: Int
  }

resp :: IR.Name
resp = "resp"

compile :: L1.Term -> [Statement]
compile t =
  let (c, bb) = evalState (_compile t) (States 0 0 0 0)
  in [SL "main"] ++ c ++ [ST (Return resp)] ++ bb


_compile :: L1.Term -> State States ([Statement], [Statement])
_compile (Num n)   = return ([SC (AssignI resp n)],[])
_compile (Ident n) = return ([SC (AssignV resp n)],[])
_compile (Fn _ t1) = do
  fn <- return "_fn_1"
  (c1,bb1) <- _compile t1
  bbs <- return $ [SL fn] ++ c1 ++ [ST (Return resp)]
  cs <- return $ [SC (AssignL resp fn)]
  return (cs, bbs ++ bb1)

_compile (App t1 t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  t1c <- return $ c1 ++ [SC (AssignV "fn" resp)]
  t2c <- return $ c2 ++ [SC (AssignV "param" resp)]
  -- env new and add
  cs <- return $ t1c ++ t2c ++ [SC (EnvNew "env_id" 0), SC (Call resp "fn" "env_id")]
  return (cs, bb1 ++ bb2)

_compile (L1.Op t1 op t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  cs <- return $ t1c ++ t2c ++ [SC (IR.Op t1Ident (setOp op) t2Ident)]
  return (cs, bb1 ++ bb2)

_compile (L1.If t1 t2 t3) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  (c3,bb3) <- _compile t3
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  t1c <- return $  c1 ++ [ST (IR.If resp thenLabel)] ++ [SL dummyLabel]
  t3c <- return $  c3 ++ [ST (Goto backLabel)] ++ [SL backLabel]
  bbThen <- return $  [SL thenLabel] ++ c2 ++ [ST (Goto backLabel)]
  cs <- return $  t1c ++ t3c
  bbs <- return $  bbThen ++ bb1 ++ bb2 ++ bb3
  return (cs, bbs)

_compile (Let n t1 t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  cs <- return $  c1 ++ [SC (AssignV n resp)] ++ c2
  return (cs, bb1 ++ bb2)

_compile (Letrec n t1 t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  (SL fn_name) <- return $  head bb1
  cs <- return $  [SC (AssignV n fn_name)] ++ c1 ++ c2
  return (cs, bb1 ++ bb2)


nextThenLabel :: State States Label
nextThenLabel = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  identN <- return (identNum s)
  put (States (thenN+1) backN dummyN identN)
  return $ "then" ++ (show thenN)

nextBackLabel :: State States Label
nextBackLabel = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  identN <- return (identNum s)
  put (States thenN (backN+1) dummyN identN)
  return $ "back" ++ (show backN)

nextDummyLabel :: State States Label
nextDummyLabel = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  identN <- return (identNum s)
  put (States thenN backN (dummyN+1) identN)
  return $ "dummy" ++ (show dummyN)

nextIdentName :: State States IR.Name
nextIdentName = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  identN <- return (identNum s)
  put (States thenN backN dummyN (identN+1))
  return $ "var" ++ (show identN)


setOp :: L1.OpCode -> IR.OpCode
setOp L1.And = IR.And
setOp L1.Or = IR.Or
setOp L1.Add = IR.Add
setOp L1.Sub = IR.Sub
setOp L1.Mult = IR.Mult
setOp L1.Equal = IR.Equal
setOp L1.NotEqual = IR.NotEqual
setOp L1.Greater = IR.Greater
setOp L1.GreaterEqual = IR.GreaterEqual
setOp L1.Lesser = IR.Lesser
setOp L1.LesserEqual = IR.LesserEqual

startRed :: String
startRed = "\x1b[31m"

startGreen :: String
startGreen = "\x1b[32m"

noColor :: String
noColor = "\x1b[0m"

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
printStatement (ST (Goto l)) = ident ++ startRed ++ "goto " ++ l ++ noColor ++ "\n"
printStatement (ST (Return name)) = ident ++ startRed ++ "return " ++ name ++ noColor ++ "\n"
printStatement (ST (IR.If name l)) = ident ++ startRed ++ "if " ++ name ++ " goto " ++ l ++ noColor ++ "\n"

printStatement (SL name) = startGreen ++ name ++ noColor ++ ":\n"

