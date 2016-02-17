module Compiler.Compile where

import Control.Monad.State

import L1.Language as L1
import AcquaIR.Language as IR

import Compiler.CompileStates
import Compiler.CompileStatement
import Compiler.Transformations.AddWaits
import Compiler.Transformations.EliminateRedundantVars


resp :: IR.Name
resp = "resp"

compile :: L1.Term -> IR.Program
compile t =
  addWaits (eliminateRedundantVars (statementsToProgram statements))
  where
    (c, bb) = evalState (_compile t) defaultCompileStates
    statements = [SL "main"] ++ c ++ [ST (Return resp)] ++ bb

_compile :: L1.Term -> State CompileStates ([Statement], [Statement])
_compile (Num n)   = return ([SC (AssignI resp n)],[])
_compile (Ident n) = return ([SC (AssignV resp n)],[])
_compile (Fn _ t1) = do
  fn <- nextFnLabel
  (c1,bb1) <- _compile t1
  bbs <- return $ [SL fn] ++ c1 ++ [ST (Return resp)]
  cs <- return $ []
  return (cs, bbs ++ bb1)

_compile (App t1 t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  paramName <- case t1 of
                  (Ident fn) -> (getFnVarName fn)
                  (Fn n _ ) -> return n
                  _ -> error "App not applying identifier or function!"

  t1c <- return $ c1 ++ [SC (AssignV "fn" resp)]
  t2c <- return $ c2 ++ []
  envs <- return $ [SC (EnvNew "env_id" 0), SC (EnvAddL "env_id" paramName resp), SC (EnvAddL "env_id" "fn" "fn")]
  cs <- return $ t1c ++ t2c ++ envs ++ [SC (Call resp "fn" "env_id")]
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
  (Fn varName _) <- return t1
  _ <- setFnVarName n varName
  (c1,bb1) <- _compile t1
  (SL fn_name) <- return $ head bb1
  (c2,bb2) <- _compile t2
  cs <- return $  [SC (AssignL n fn_name)] ++ c1 ++ c2
  return (cs, bb1 ++ bb2)


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