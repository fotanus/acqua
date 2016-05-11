module Compiler.Compile where

import Control.Monad.State

import UL1.Language as UL1
import AcquaIR.Language as IR

import Compiler.CompileStates
import Compiler.CompileStatement
import Compiler.Transformations.AddWaits
import Compiler.Transformations.EliminateRedundantVars

resp :: IR.Name
resp = "resp"

compile :: UL1.Term -> IR.Program
compile t =
  addWaits (eliminateRedundantVars (statementsToProgram statements))
  where
    (c, bb) = evalState (_compile t) defaultCompileStates
    statements = [SL "main"] ++ c ++ [ST (Return resp)] ++ bb


_compile :: UL1.Term -> State CompileStates ([Statement], [Statement])
_compile (Param _) = error "Compiler not implemented for Param"
_compile (Num n)   = return ([SC (AssignI resp n)],[])
_compile (Ident n) = return ([SC (AssignV resp n)],[])

_compile (Fn params t1) = do
  fn <- nextFnLabel
  (c1,bb1) <- _compile t1
  bbs <- return $ [SL fn] ++ c1 ++ [ST (Return resp)]
  cs <- return $ [
                   SC (NewClosure "closure" (length params)),
                   SC (SetClosureFn "closure" fn),
                   SC (SetClosureMissingI "closure" (length params)),
                   SC (SetClosureCountI "closure" 0),
                   SC (AssignL resp "closure")
                 ]
  return (cs, bbs ++ bb1)

_compile (App t1 t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  envs <- return  [
                    SC (AssignV "param" resp),
                    SC (AssignI "one" 1),
                    SC (GetClosureMissing "closure" "missing"),
                    SC (GetClosureCount "closure" "count"),
                    SC (IR.Op "missing" IR.Sub "one"),
                    SC (AssignV "new_missing" resp),
                    SC (IR.Op "count" IR.Add "one"),
                    SC (AssignV "new_count" resp),
                    SC (SetClosureCount "closure" "new_count"),
                    SC (SetClosureMissing "closure" "new_missing"),
                    SC (SetClosureParam "closure" "count" "param"),
                    ST (IR.If resp thenLabel),
                    SL dummyLabel,
                    SC (AssignV "resp" "closure"),
                    ST (Goto backLabel),
                    SL backLabel
                  ]
  bbThen <- return $ [
                       SL thenLabel,
                       SC (Call "resp" "closure"),
                       ST (Goto backLabel)
                     ]
  cs <- return $ c1 ++ [SC (AssignV "closure" resp)] ++ c2 ++ envs
  return (cs, bbThen ++ bb1 ++ bb2)

_compile (UL1.Op t1 op t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  cs <- return $ t1c ++ t2c ++ [SC (IR.Op t1Ident (setOp op) t2Ident)]
  return (cs, bb1 ++ bb2)

_compile (UL1.If t1 t2 t3) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  (c3,bb3) <- _compile t3
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  t1c <- return $ c1 ++ [ST (IR.If resp thenLabel)] ++ [SL dummyLabel]
  t3c <- return $ c3 ++ [ST (Goto backLabel)] ++ [SL backLabel]
  bbThen <- return $ [SL thenLabel] ++ c2 ++ [ST (Goto backLabel)]
  cs <- return $ t1c ++ t3c
  bbs <- return $ bbThen ++ bb1 ++ bb2 ++ bb3
  return (cs, bbs)

_compile (Let n t1 t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  cs <- return $  c1 ++ [SC (AssignV n resp)] ++ c2
  return (cs, bb1 ++ bb2)

_compile (Letrec n t1 t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  cs <- return $  c1 ++ [SC (AssignV n resp)] ++ c2
  return (cs, bb1 ++ bb2)


setOp :: UL1.OpCode -> IR.OpCode
setOp UL1.And = IR.And
setOp UL1.Or = IR.Or
setOp UL1.Add = IR.Add
setOp UL1.Sub = IR.Sub
setOp UL1.Mult = IR.Mult
setOp UL1.Equal = IR.Equal
setOp UL1.NotEqual = IR.NotEqual
setOp UL1.Greater = IR.Greater
setOp UL1.GreaterEqual = IR.GreaterEqual
setOp UL1.Lesser = IR.Lesser
setOp UL1.LesserEqual = IR.LesserEqual
