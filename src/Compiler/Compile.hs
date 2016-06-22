module Compiler.Compile where

import Logger
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
    (c, bb) = evalState (_compile (fillFreeVars t)) defaultCompileStates
    statements = [SL "main"] ++ c ++ [ST (Return resp)] ++ bb


_compile :: L1.Term -> State CompileStates ([Statement], [Statement])
_compile (Num n)   = return ([SC (AssignI resp n)],[])
_compile (Ident n) = do
    c <- getClosureInfo n
    return $ case c of
               Nothing -> ([SC (AssignV resp n)],[])
               Just (fn,_,params,vars,False) -> ([
                           SC (NewClosure "closure" ((length vars')+(length params))),
                           SC (SetClosureFn "closure" fn)
                         ] ++ freeVarsSetParams ++ [
                           SC (SetClosureMissingI "closure" (length params)),
                           SC (SetClosureCountI "closure" ((length vars'))),
                           SC (AssignV resp "closure")
                         ],[])
                        where
                          vars' = if null vars then [] else tail vars
                          freeVarsSetParams = map (\(v,idx) -> SC (SetClosureParamI "closure" idx v)) (zip vars [0..])
               Just (fn,_,params,vars,True) -> ([
                           SC (NewClosure "closure" ((length vars')+(length params)+1)),
                           SC (SetClosureFn "closure" fn),
                           SC (SetClosureParamIL "closure" 0 fn)
                         ] ++ freeVarsSetParams ++ [
                           SC (SetClosureMissingI "closure" 1),
                           SC (SetClosureCountI "closure" ((length vars')+(length params))),
                           SC (AssignV resp "closure")
                         ],[])
                        where
                          vars' = if null vars then [] else tail vars
                          freeVarsSetParams = map (\(v,idx) -> SC (SetClosureParamI "closure" idx v)) (zip vars [1..])

_compile (Fn params t1 freeVars) = do
  fn <- nextFnLabel
  (c1,bb1) <- _compile t1
  freeVarsSetParams <- return $ map (\(v,idx) -> SC (SetClosureParamI "closure" idx v)) (zip freeVars [0..])
  getParamsCommands <- return $ map (\p-> SC (GetClosureParam "closure" (snd p) (fst p))) (zip freeVars [0..])
  getParamsCommands' <- return $ getParamsCommands ++ (map (\p-> SC (GetClosureParam "closure" (snd p) (fst p))) (zip params [(length getParamsCommands)..] ))
  newClosureCommands <- return $ [
                                   SC (NewClosure "closure" ((length freeVars)+(length params))),
                                   SC (SetClosureFn "closure" fn)
                                 ] ++ freeVarsSetParams ++ [
                                   SC (SetClosureMissingI "closure" (length params)),
                                   SC (SetClosureCountI "closure" (length freeVars)),
                                   SC (AssignV resp "closure")
                                 ]
  fnBody <- return $ [SL fn] ++ getParamsCommands' ++ c1 ++ [ST (Return resp)]
  return (newClosureCommands, fnBody++ bb1)


_compile (App t1 t2) = do
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  closureIdent <- nextIdentName
  closureIdent' <- nextIdentName
  envs <- return  [
                    SC (AssignV "param" resp),
                    SC (AssignI "one" 1),
                    SC (AssignV closureIdent' closureIdent'),
                    SC (GetClosureMissing closureIdent' "missing"),
                    SC (GetClosureCount closureIdent' "count"),
                    SC (IR.Op "missing" IR.Sub "one"),
                    SC (AssignV "new_missing" resp),
                    SC (IR.Op "count" IR.Add "one"),
                    SC (AssignV "new_count" resp),
                    SC (SetClosureCount closureIdent' "new_count"),
                    SC (SetClosureMissing closureIdent' "new_missing"),
                    SC (SetClosureParam closureIdent' "count" "param"),
                    SC (IR.Op "new_missing" IR.Lesser "one"),
                    ST (IR.If resp thenLabel),
                    SL dummyLabel,
                    SC (AssignV "resp" closureIdent'),
                    ST (Goto backLabel),
                    SL backLabel
                  ]
  bbThen <- return $ [
                       SL thenLabel,
                       SC ( AssignV closureIdent closureIdent'),
                       SC (Call "resp" closureIdent),
                       ST (Goto backLabel)
                     ]
  cs <- return $ c1 ++ [SC (AssignV closureIdent' resp)] ++ c2 ++ envs
  return (cs, bbThen ++ bb1 ++ bb2)

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
  t1c <- return $ c1 ++ [ST (IR.If resp thenLabel)] ++ [SL dummyLabel]
  t3c <- return $ c3 ++ [ST (Goto backLabel)] ++ [SL backLabel]
  bbThen <- return $ [SL thenLabel] ++ c2 ++ [ST (Goto backLabel)]
  cs <- return $ t1c ++ t3c
  bbs <- return $ bbThen ++ bb1 ++ bb2 ++ bb3
  return (cs, bbs)

_compile (Let n t1 t2) = do
  _ <- addKnownVars n
  (c1,bb1) <- _compile t1
  (c2,bb2) <- _compile t2
  cs <- return $  c1 ++ [SC (AssignV n resp)] ++ c2
  return (cs, bb1 ++ bb2)

_compile (Letrec n (Fn params t1 freeVars) t2) = do
  _ <- addKnownVars n
  freeVars' <- return $ traceShow freeVars $ traceShow n $ traceShowId $ delete n freeVars
  fn <- nextFnLabel
  _ <- setClosureInfo n (fn, n, params, freeVars', True)
  (c1,bb1) <- _compile t1
  getParamsCommands <- return $ [SC (GetClosureParam "closure" 0 n)]
  getParamsCommands' <- return $ getParamsCommands ++ map (\p-> SC (GetClosureParam "closure" (snd p) (fst p))) (zip freeVars' [1..])
  getParamsCommands'' <- return $ getParamsCommands' ++ (map (\p-> SC (GetClosureParam "closure" (snd p) (fst p))) (zip params [(length getParamsCommands')..] ))
  fnBody <- return $ [SL fn] ++ getParamsCommands'' ++ c1 ++ [ST (Return resp)]
  (c2,bb2) <- _compile t2
  return (c2, fnBody ++ bb1 ++ bb2)

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
