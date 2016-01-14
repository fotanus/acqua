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
  in [(BB "main" 0 c (Return resp))] ++ bb


_compile :: L1.Term -> ([IR.Command], [IR.BasicBlock])
_compile (Num n)   = ([AssignI resp n],[])
_compile (Ident n) = ([AssignV resp n],[])

_compile (Fn n t1) =
  let
    fn = "_fn_" ++ n
    (c1,bb1) = _compile t1
    bbs = [(BB fn 0 c1 (Return resp))]
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
    cs = t1c ++ t2c ++ [Call resp "fn" "env"]
  in
    (cs, bb1 ++ bb2)

_compile (L1.Op t1 _ t2) =
  let
    (c1,bb1) = _compile t1
    (c2,bb2) = _compile t2
    t1c = c1 ++ [AssignV t1Ident resp]
    t2c = c2 ++ [AssignV t2Ident resp]
    cs = t1c ++ t2c ++ [IR.Op t1Ident IR.Add t2Ident]
  in
    (cs, bb1 ++ bb2)

_compile (L1.If t1 t2 t3) =
  let
    (c1,bb1) = _compile t1
    (c2,bb2) = _compile t2
    (c3,bb3) = _compile t2
    t1c = c1 ++ [AssignV t1Ident resp]
    t2c = c2 ++ [AssignV t2Ident resp]
    cs = t1c ++ t2c ++ [IR.Op t1Ident IR.Add t2Ident]
  in
    (cs, bb1 ++ bb2)

_compile (Let n t1 t2) =
  let
    (c1,bb1) = _compile t1
    (c2,bb2) = _compile t2
    cs = c1 ++ [AssignV n resp] ++ c2
  in
    (cs, bb1 ++ bb2)

_compile (Letrec n t1 t2) =
  let
    fn = "_fn_" ++ n
    (c1,bb1) = _compile t1
    (c2,bb2) = _compile t2
    cs = [AssignV n fn] ++ c2
    bbs = [(BB fn 0 c1 (Return resp))]
  in
    (cs, bb1 ++ bb2 ++ bbs)
