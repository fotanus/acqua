module AcquaIR.Compile where

import L1.Language as L1
import AcquaIR.Language as IR

resp :: IR.Name
resp = "resp"

t1Ident :: IR.Name
t1Ident = "t1"

t2Ident :: IR.Name
t2Ident = "t2"

compile :: L1.Term -> IR.Program
compile term =
  let (c,bb) = _compile term
  in [(BB 0  ([CLabel "main"] ++ c ++ [Return resp]))] ++ bb


_compile :: L1.Term -> ([IR.Command], [IR.BasicBlock])
_compile (Num n)   = ([AssignI resp n],[])
_compile (Ident n) = ([AssignV resp n],[])

_compile (Fn _ t1) =
  let
    fn = "_fn_1"
    (c1,bb1) = _compile t1
    bbs = [BB 0 ([CLabel fn] ++ c1 ++ [Return resp])]
    cs = [AssignL resp fn]
  in
    (cs, bbs ++ bb1)

_compile (App t1 t2) =
  let
    (c1,bb1) = _compile t1
    (c2,bb2) = _compile t2
    t1c = c1 ++ [AssignV "fn" resp]
    t2c = c2 ++ [AssignV "param" resp]
    -- env new and add
    cs = t1c ++ t2c ++ [ EnvNew "env_id" 0, Call resp "fn" "env_id" ]
  in
    (cs, bb1 ++ bb2)

_compile (L1.Op t1 op t2) =
  let
    (c1,bb1) = _compile t1
    (c2,bb2) = _compile t2
    t1c = c1 ++ [AssignV t1Ident resp]
    t2c = c2 ++ [AssignV t2Ident resp]
    cs = t1c ++ t2c ++ [IR.Op t1Ident (setOp op) t2Ident]
  in
    (cs, bb1 ++ bb2)

_compile (L1.If t1 t2 t3) =
  let
    (c1,bb1) = _compile t1
    (c2,bb2) = _compile t2
    (c3,bb3) = _compile t3
    t1c = c1 ++ [IR.If resp "then"]
    t3c = c3 ++ [CLabel "back"]
    bbThen = [BB 0 ([CLabel "then"] ++ c2 ++ [Goto "back"])]
    cs = t1c ++ t3c
    bbs = bbThen ++ bb1 ++ bb2 ++ bb3
  in
    (cs, bbs)

_compile (Let n t1 t2) =
  let
    (c1,bb1) = _compile t1
    (c2,bb2) = _compile t2
    cs = c1 ++ [AssignV n resp] ++ c2
  in
    (cs, bb1 ++ bb2)

_compile (Letrec n t1 t2) =
  let
    (c1,bb1) = _compile t1
    (c2,bb2) = _compile t2
    (CLabel fn_name) = head (commands (head bb1))
    cs = [AssignV n fn_name] ++ c2
  in
    (cs, bb1 ++ bb2)

setOp :: L1.OpCode -> IR.OpCode
setOp L1.Add = IR.Add
setOp L1.Mult = IR.Mult
setOp L1.And = IR.Add
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
