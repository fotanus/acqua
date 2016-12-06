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
  addSplitMap.addWaits.eliminateRedundantVars.statementsToProgram $ statements
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
                           SC (NewCallRecord "callRecord" ((length vars)+(length params)+1)),
                           SC (SetCallRecordFn "callRecord" fn),
                           SC (SetCallRecordParamIL "callRecord" 0 fn)
                         ] ++ freeVarsSetParams ++ [
                           SC (SetCallRecordMissingI "callRecord" (length params)),
                           SC (SetCallRecordCountI "callRecord" ((length vars)+1)),
                           SC (AssignV resp "callRecord")
                         ]
                        where
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


_compile (MultiApp t1 t2) = do
  c1 <- _compile t1
  c2s <- mapM _compile t2
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  callRecordIdent <- nextIdentName
  callRecordIdent' <- nextIdentName
  nparams <- nextIdentName
  currentCount <- nextIdentName
  addParams <- return $ concat $ map (\c-> c ++ [
                                         SC (AssignV "param" "resp"),
                                         SC (IR.Op currentCount currentCount IR.Add "one"),
                                         SC (SetCallRecordParam callRecordIdent' currentCount "param")
                                       ]
                            ) c2s

  bbThen <- return $ [
                       SL thenLabel,
                       SC (AssignV callRecordIdent callRecordIdent'),
                       SC (Call "resp" callRecordIdent),
                       ST (Goto backLabel)
                     ]
  envs <- return $ [
                     SC (AssignI "one" 1),
                     SC (AssignI nparams (length t2)),
                     SC (AssignV callRecordIdent' callRecordIdent'),
                     SC (GetCallRecordMissing callRecordIdent' "missing"),
                     SC (GetCallRecordCount callRecordIdent' "count"),
                     SC (IR.Op currentCount "count" IR.Sub "one"),
                     SC (IR.Op "resp" "missing" IR.Sub nparams),
                     SC (AssignV "new_missing" resp),
                     SC (IR.Op "resp" "count" IR.Add nparams),
                     SC (AssignV "new_count" resp),
                     SC (SetCallRecordCount callRecordIdent' "new_count"),
                     SC (SetCallRecordMissing callRecordIdent' "new_missing")
                   ] ++ addParams ++ [
                     SC (IR.Op "resp" "new_missing" IR.Lesser "one"),
                     ST (IR.If resp thenLabel),
                     SL dummyLabel,
                     SC (AssignV "resp" callRecordIdent'),
                     ST (Goto backLabel)
                   ] ++ bbThen ++ [
                     SL backLabel
                   ]
  cs <- return $ c1 ++ [SC (AssignV callRecordIdent' resp)] ++ envs
  return cs


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
  listIdent <- nextIdentName
  listSets <- return $ listSetsFun elements listIdent 0
  return $ [ SC (NewList listIdent (length elements)) ] ++ listSets ++ [SC (AssignV resp listIdent)]

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
  assigns <- return $ [(SC (AssignV t1Ident t1Ident)), (SC (AssignV t2Ident t2Ident))]
  cs <- return $ t1c ++ t2c ++ assigns ++ [SC (IR.Concat "resp" t1Ident t2Ident)]
  return cs

_compile (L1.Concat3 t1 t2 t3) = do
  c1 <- _compile t1
  c2 <- _compile t2
  c3 <- _compile t3
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t3Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  t3c <- return $ c3 ++ [SC (AssignV t3Ident resp)]
  assigns <- return $ [(SC (AssignV t1Ident t1Ident)), (SC (AssignV t2Ident t2Ident)), (SC (AssignV t3Ident t3Ident))]
  cs <- return $ t1c ++ t2c ++ t3c ++ assigns ++ [SC (IR.Concat3 "resp" t1Ident t2Ident t3Ident)]
  return cs

_compile (L1.Slice t1 t2 t3) = do
  c1 <- _compile t1
  c2 <- _compile t2
  c3 <- _compile t3
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t3Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  t3c <- return $ c3 ++ [SC (AssignV t3Ident resp)]
  cs <- return $ t1c ++ t2c ++ t3c ++ [SC (IR.Slice "resp" t1Ident t2Ident t3Ident)]
  return cs

_compile (L1.Map t1 t2) = do
  c1 <- _compile t1
  c2 <- _compile t2
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  thenLabel <- nextThenLabel
  thenLabel2 <- nextThenLabel
  thenLabel3 <- nextThenLabel
  dummyLabel <- nextDummyLabel
  dummyLabel2 <- nextDummyLabel
  dummyLabel3 <- nextDummyLabel
  mapCode <- return $ [
      -- if (length list) > pus * 4
      SC (GetNPU "pus"),
      SC (IR.Length "listSize" t2Ident),
      SC (AssignI "four" 2),
      SC (IR.Op "resp" "pus" IR.Mult "four"),
      SC (IR.Op "resp" "listSize" IR.Greater "resp"),
      ST (IR.If "resp" thenLabel),

      -- else map wait
      SL dummyLabel,
      SC (IR.Map "resp" t1Ident t2Ident),
      SC Wait,
      ST (Goto dummyLabel3),

      -- then distribute
      -- split the list in n lists
      -- create one job for each list
      SL thenLabel,
      -- SC (IR.Op "sliceSize" "listSize" IR.Div "pus"),
      -- SC (IR.Op "numberOfResults" "listSize" IR.Div "sliceSize"),
      SC (IR.AssignI "two" 1),
      SC (IR.Op "numberOfResults" "pus" IR.Div "two"),
      SC (IR.Op "sliceSize" "listSize" IR.Div "numberOfResults"),
      SC (NewList "partialResultLists" 100000),
      SC (IR.AssignI "start" 0),
      SC (IR.AssignV "end" "sliceSize"),
      SC (IR.AssignI "idx" 0),
      SC (IR.AssignI "one" 1),
      ST (Goto thenLabel2),

      SL thenLabel2,
      SC (IR.Slice "list" t2Ident  "start" "end"),
      SC (NewCallRecord "callRecord" 2),
      SC (SetCallRecordFn "callRecord" "splitMap"),
      SC (SetCallRecordMissingI "callRecord" 0),
      SC (SetCallRecordCountI "callRecord" 2),
      SC (SetCallRecordParamI "callRecord" 0 t1Ident),
      SC (SetCallRecordParamI "callRecord" 1 "list"),
      SC (CallL "partialResultLists" "idx" "callRecord"),
      SC (IR.Op "start" "start" IR.Add "sliceSize"),
      SC (IR.Op "end" "end" IR.Add "sliceSize"),
      SC (IR.Op "idx" "idx" IR.Add "one"),
      SC (IR.Op "resp" "listSize" IR.Greater "start"),
      ST (IR.If "resp" thenLabel2),

      -- merge lists setup
      SL dummyLabel2,
      SC (NewList "resultList" 0),
      SC (AssignI "idx" 0),
      SC (Wait),
      ST (Goto thenLabel3),

      -- merge lists loop
      SL thenLabel3,
      SC (ListGet "partialList" "partialResultLists" "idx"),
      SC (AssignV "partialList" "partialList"),
      SC (IR.Concat "resultList" "resultList" "partialList"),
      SC (IR.Op "idx" "idx" IR.Add "one"),
      SC (IR.Op "test" "idx" IR.Lesser "numberOfResults"),
      SC (AssignV "resp" "resultList"),
      ST (IR.If "test" thenLabel3),

      -- continue execution
      SL dummyLabel3
    ]
  cs <- return $ t1c ++ t2c ++ mapCode
  return cs

_compile (L1.Filter t1 t2) = do
  c1 <- _compile t1
  c2 <- _compile t2
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t3Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  cs <- return $ t1c ++ t2c ++ [SC (IR.Map t3Ident t1Ident t2Ident), SC Wait, SC (IR.Filter "resp" t3Ident t2Ident)]
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

  freeVarsSetParams <- return $ (SC (SetCallRecordParamIL "callRecord" 0 fn)): (map (\(v,idx) -> SC (SetCallRecordParamI "callRecord" idx v)) (zip freeVars' [1..]))
  
  newCallRecordCommands <- return $ [
                                   SC (NewCallRecord "callRecord" ((length freeVars)+(length params))),
                                   SC (SetCallRecordFn "callRecord" fn)
                                 ] ++ freeVarsSetParams ++ [
                                   SC (SetCallRecordMissingI "callRecord" (length params)),
                                   SC (SetCallRecordCountI "callRecord" (length freeVars)),
                                   SC (AssignV n "callRecord")
                                 ]

  return $ newCallRecordCommands ++ c2 ++ [ST (Goto continueLabel)] ++ fnBody ++ [(SL continueLabel)]

_compile (Letrec _ _ _) = error "Letrec first term should be a function"


listSetsFun :: [ListItem] -> String -> Int -> [Statement]
listSetsFun elements prevListVar count =  concat $ map (\(e,idx) ->
                                  case e of
                                    ListNum n -> [SC (ListSet prevListVar idx n)];
                                    ListIdent n -> [SC (ListSetN prevListVar idx n)];
                                    RecList l ->
                                      let
                                        listVar = ("recList" ++ (show count))
                                        listSets = listSetsFun l listVar (count + 1)
                                      in
                                        [ SC (NewList listVar (length l)) ]  ++ listSets ++ [SC (ListSetN prevListVar idx listVar)];
                                 ) (zip elements [0..])


addSplitMap :: IR.Program -> IR.Program
addSplitMap p =
  let
    mapSplitCommands = [
      GetCallRecordParam "callRecord" 0 "fn",
      GetCallRecordParam "callRecord" 1 "list",
      AssignV "fn" "fn",
      AssignV "list" "list",
      IR.Map "resp" "fn" "list",
      Wait
      ]
  in
    p ++ [(BB "splitMap" 0 mapSplitCommands (Return "resp"))]




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
