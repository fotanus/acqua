module Compiler.Compile where

import Data.List
import Control.Monad.State

import L1.Language as L1
import AcquaIR.Language as IR

import Compiler.CompileStates
import Compiler.CompileStatement
import Compiler.Transformations.AddWaits
import Compiler.Transformations.EliminateRedundantVars
import Compiler.Transformations.FillFreeVars

resp :: IR.Name
resp = "resp"

compile :: L1.Term -> IR.Program
compile t =
  addWaits (eliminateRedundantVars (statementsToProgram statements))
  where
    c = evalState (_compile (fillFreeVars t)) defaultCompileStates
    statements = [SL "main"] ++ c ++ [ST (Return resp)]


_compile :: L1.Term -> State CompileStates [Statement]
_compile (Num n)   = return [SC (AssignI resp n)]
_compile (Ident n) = do
    c <- getCallRecordInfo n
    return $ case c of
               Nothing -> [SC (AssignV resp n)]
               Just (fn,_,params,vars,False) -> [
                           SC (NewCallRecord "callRecord" ((length vars)+(length params))),
                           SC (SetCallRecordFn "callRecord" fn)
                         ] ++ freeVarsSetParams ++ [
                           SC (SetCallRecordMissingI "callRecord" (length params)),
                           SC (SetCallRecordCountI "callRecord" ((length vars))),
                           SC (AssignV resp "callRecord")
                         ]
                        where
                          freeVarsSetParams = map (\(v,idx) -> SC (SetCallRecordParamI "callRecord" idx v)) (zip vars [0..])
               Just (fn,_,params,vars,True) -> [
                           SC (NewCallRecord "callRecord" ((length vars')+(length params)+1)),
                           SC (SetCallRecordFn "callRecord" fn),
                           SC (SetCallRecordParamIL "callRecord" 0 fn)
                         ] ++ freeVarsSetParams ++ [
                           SC (SetCallRecordMissingI "callRecord" 1),
                           SC (SetCallRecordCountI "callRecord" ((length vars')+(length params))),
                           SC (AssignV resp "callRecord")
                         ]
                        where
                          vars' = if null vars then [] else tail vars
                          freeVarsSetParams = map (\(v,idx) -> SC (SetCallRecordParamI "callRecord" idx v)) (zip vars [1..])

_compile (Fn params t1 freeVars) = do
  fn <- nextFnLabel
  c1 <- _compile t1
  freeVarsSetParams <- return $ map (\(v,idx) -> SC (SetCallRecordParamI "callRecord" idx v)) (zip freeVars [0..])
  getParamsCommands <- return $ map (\p-> SC (GetCallRecordParam "callRecord" (snd p) (fst p))) (zip freeVars [0..])
  getParamsCommands' <- return $ getParamsCommands ++ (map (\p-> SC (GetCallRecordParam "callRecord" (snd p) (fst p))) (zip params [(length getParamsCommands)..] ))
  fnBody <- return $ [SL fn] ++ getParamsCommands' ++ c1 ++ [ST (Return resp)]
  continueLabel <- nextContinueLabel
  newCallRecordCommands <- return $ [
                                   SC (NewCallRecord "callRecord" ((length freeVars)+(length params))),
                                   SC (SetCallRecordFn "callRecord" fn)
                                 ] ++ freeVarsSetParams ++ [
                                   SC (SetCallRecordMissingI "callRecord" (length params)),
                                   SC (SetCallRecordCountI "callRecord" (length freeVars)),
                                   SC (AssignV resp "callRecord"),
                                   ST (Goto  continueLabel)
                                 ] ++ fnBody ++ [
                                  SL continueLabel
                                 ]
  return newCallRecordCommands


_compile (App t1 t2) = do
  c1 <- _compile t1
  c2 <- _compile t2
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  callRecordIdent <- nextIdentName
  callRecordIdent' <- nextIdentName
  bbThen <- return $ [
                       SL thenLabel,
                       SC (AssignV callRecordIdent callRecordIdent'),
                       SC (Call "resp" callRecordIdent),
                       ST (Goto backLabel)
                     ]
  envs <- return $ [
                    SC (AssignV "param" resp),
                    SC (AssignI "one" 1),
                    SC (AssignV callRecordIdent' callRecordIdent'),
                    SC (GetCallRecordMissing callRecordIdent' "missing"),
                    SC (GetCallRecordCount callRecordIdent' "count"),
                    SC (IR.Op "resp" "missing" IR.Sub "one"),
                    SC (AssignV "new_missing" resp),
                    SC (IR.Op "resp" "count" IR.Add "one"),
                    SC (AssignV "new_count" resp),
                    SC (SetCallRecordCount callRecordIdent' "new_count"),
                    SC (SetCallRecordMissing callRecordIdent' "new_missing"),
                    SC (SetCallRecordParam callRecordIdent' "count" "param"),
                    SC (IR.Op "resp" "new_missing" IR.Lesser "one"),
                    ST (IR.If resp thenLabel),
                    SL dummyLabel,
                    SC (AssignV "resp" callRecordIdent'),
                    ST (Goto backLabel)
                  ] ++ bbThen ++ [
                    SL backLabel
                  ]
  cs <- return $ c1 ++ [SC (AssignV callRecordIdent' resp)] ++ c2 ++ envs
  return cs

_compile (L1.List elements) = do
  return $ [ SC (NewList resp (length elements)) ] ++
           map (\(e,idx)-> SC (ListSet resp idx e)) (zip elements [0..])

_compile (L1.Head t1) = do
  c1 <- _compile t1
  return $ c1 ++ [SC (IR.Head resp resp)]

_compile (L1.Tail t1) = do
  c1 <- _compile t1
  return $ c1 ++ [SC (IR.Tail resp resp)]

_compile (L1.Last t1) = do
  c1 <- _compile t1
  return $ c1 ++ [SC (IR.Last resp resp)]

_compile (L1.Concat t1 t2) = do
  c1 <- _compile t1
  c2 <- _compile t2
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  cs <- return $ t1c ++ t2c ++ [SC (IR.Concat "resp" t1Ident t2Ident)]
  return cs

_compile (L1.Length t1) = do
  c1 <- _compile t1
  return $ c1 ++ [SC (IR.Length resp resp)]

_compile (L1.Op t1 op t2) = do
  c1 <- _compile t1
  c2 <- _compile t2
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  cs <- return $ t1c ++ t2c ++ [SC (IR.Op "resp" t1Ident (setOp op) t2Ident)]
  return cs

_compile (L1.If t1 t2 t3) = do
  c1 <- _compile t1
  c2 <- _compile t2
  c3 <- _compile t3
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  continueLabel <- nextContinueLabel
  t1c <- return $ c1 ++ [ST (IR.If resp thenLabel)] ++ [SL dummyLabel]
  t3c <- return $ c3 ++ [ST (Goto backLabel)] ++ [SL backLabel]
  bbThen <- return $ [SL thenLabel] ++ c2 ++ [ST (Goto backLabel)]
  cs <- return $ t1c ++ t3c ++ [ST (Goto continueLabel)] ++ bbThen ++ [(SL continueLabel)]
  return cs

_compile (Let n (Fn params t1 freeVars) t2) = do
  _ <- addKnownVars n
  fn <- nextFnLabel
  c1 <- _compile t1
  freeVarsSetParams <- return $ map (\(v,idx) -> SC (SetCallRecordParamI "callRecord" idx v)) (zip freeVars [0..])
  getParamsCommands <- return $ map (\p-> SC (GetCallRecordParam "callRecord" (snd p) (fst p))) (zip freeVars [0..])
  getParamsCommands' <- return $ getParamsCommands ++ (map (\p-> SC (GetCallRecordParam "callRecord" (snd p) (fst p))) (zip params [(length getParamsCommands)..] ))
  fnBody <- return $ [SL fn] ++ getParamsCommands' ++ c1 ++ [ST (Return resp)]
  continueLabel <- nextContinueLabel
  newCallRecordCommands <- return $ [
                                   SC (NewCallRecord "callRecord" ((length freeVars)+(length params))),
                                   SC (SetCallRecordFn "callRecord" fn)
                                 ] ++ freeVarsSetParams ++ [
                                   SC (SetCallRecordMissingI "callRecord" (length params)),
                                   SC (SetCallRecordCountI "callRecord" (length freeVars)),
                                   SC (AssignV resp "callRecord"),
                                   ST (Goto  continueLabel)
                                 ] ++ fnBody ++ [
                                  SL continueLabel
                                 ]
  _ <- setCallRecordInfo n (fn, n, params, freeVars, False)
  c2 <- _compile t2
  cs <- return $  newCallRecordCommands ++ [SC (AssignV n resp)] ++ c2
  return cs

_compile (Let n t1 t2) = do
  _ <- addKnownVars n
  c1 <- _compile t1
  c2 <- _compile t2
  cs <- return $  c1 ++ [SC (AssignV n resp)] ++ c2
  return cs

_compile (Letrec n (Fn params t1 freeVars) t2) = do
  _ <- addKnownVars n
  freeVars' <- return $ delete n freeVars
  fn <- nextFnLabel
  _ <- setCallRecordInfo n (fn, n, params, freeVars', True)
  continueLabel <- nextContinueLabel
  c1 <- _compile t1
  getParamsCommands <- return $ [SC (GetCallRecordParam "callRecord" 0 n)]
  getParamsCommands' <- return $ getParamsCommands ++ map (\p-> SC (GetCallRecordParam "callRecord" (snd p) (fst p))) (zip freeVars' [1..])
  getParamsCommands'' <- return $ getParamsCommands' ++ (map (\p-> SC (GetCallRecordParam "callRecord" (snd p) (fst p))) (zip params [(length getParamsCommands')..] ))
  fnBody <- return $ [SL fn] ++ getParamsCommands'' ++ c1 ++ [ST (Return resp)]
  c2 <- _compile t2
  return $ c2 ++ [ST (Goto continueLabel)] ++ fnBody ++ [(SL continueLabel)]

_compile (Letrec _ _ _) = error "Letrec first term should be a function"


setOp :: L1.OpCode -> IR.OpCode
setOp L1.And = IR.And
setOp L1.Or = IR.Or
setOp L1.Add = IR.Add
setOp L1.Sub = IR.Sub
setOp L1.Mult = IR.Mult
setOp L1.Div = IR.Div
setOp L1.Equal = IR.Equal
setOp L1.NotEqual = IR.NotEqual
setOp L1.Greater = IR.Greater
setOp L1.GreaterEqual = IR.GreaterEqual
setOp L1.Lesser = IR.Lesser
setOp L1.LesserEqual = IR.LesserEqual
