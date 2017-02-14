module Compiler.Compile where

import Logger
import Control.Monad.State

import L1.Language as L1
import L1.Type
import AcquaIR.Language as IR

import Compiler.CompileStates
import Compiler.CompileStatement
import Compiler.Transformations.AddWaits
import Compiler.Transformations.EliminateRedundantVars
import Compiler.Transformations.FillFreeVars

resp :: IR.Name
resp = "resp"

compile :: L1.Term -> Int -> IR.Program
compile t opt =
  addSplitMap.addWaits.eliminateRedundantVars.statementsToProgram $ statements
  where
    c = evalState (_compile (fillFreeVars (fillTypes t [])) opt) defaultCompileStates
    statements = [SL "main"] ++ c ++ [ST (Return resp)]


_compile :: L1.Term -> Int -> State CompileStates [Statement]
_compile (Num n _) _ = return [SC (AssignI resp n)]
_compile (Ident n _) _ = do
  c1 <- getFromSymbolTable n
  return c1

_compile (Fn params t1 (typ,freeVars)) opt = do
  FnT paramTypes _ <- return typ
  _ <- forM [0..((length params)-1)] $ \i ->
    setSymbolTable (params!!i) $ case typ of
                                 FnT paramT _ -> if (paramT!!i) == IntT || (params!!i) == "arg"
                                                 then [SC (AssignV "resp" (params!!i))]
                                                 else [SC (InnerCopy "resp" (params!!i))]
                                 _             -> error $ "Applying function that don't have type FnT: " ++ (show typ)
  fn <- nextFnLabel
  c1 <- _compile t1 opt
  callRecordIdent <- nextIdentName
  innerCopyTempId <- nextIdentName
  freeVarsSetParams <- return $ concat $ map (\((fv,typ'),i)-> if typ' == IntT || fv  == "arg"
                                                               then [SC (SetCallRecordParamI callRecordIdent i fv)]
                                                               else [SC (InnerCopy innerCopyTempId fv), SC (SetCallRecordParamI callRecordIdent i innerCopyTempId)]
                                             ) (zip freeVars [0..])
  getParamsCommands <- return $ map (\((fv,_),i) -> SC (GetCallRecordParam "callRecord" i fv)) (zip freeVars [0..])
  getParamsCommands' <- return $ getParamsCommands ++ (map (\p-> SC (GetCallRecordParam "callRecord" (snd p) (fst p))) (zip params [(length getParamsCommands)..] ))
  refParamsIdxs <- return $ filter (\i -> (paramTypes!!i) /= IntT && ((params!!i) /= "arg")) [0..((length paramTypes)-1)]
  outerCopyCmds <- return $ map (\a-> SC (OuterCopy a a)) $ map (\i->params!!i) refParamsIdxs
  outerCopyCmds' <- return $ outerCopyCmds ++ (map (\a-> SC (OuterCopy a a)) (map (\(a,_) -> a) (filter (\(p,t) -> t /= IntT && (p /= "arg")) freeVars)))
  deleteCmds <- return $ map (\a-> SC (Delete a )) (map (\i->params!!i) refParamsIdxs)
  fnBody <- return $ [SL fn] ++ getParamsCommands' ++ outerCopyCmds' ++ c1 ++ deleteCmds ++ [ST (Return resp)]
  continueLabel <- nextContinueLabel
  newCallRecordCommands <- return $ [
                                   SC (NewCallRecord callRecordIdent ((length freeVars)+(length params))),
                                   SC (SetCallRecordFn callRecordIdent fn)
                                 ] ++ freeVarsSetParams ++ [
                                   SC (SetCallRecordMissingI callRecordIdent (length params)),
                                   SC (SetCallRecordCountI callRecordIdent (length freeVars)),
                                   SC (AssignV resp callRecordIdent),
                                   ST (Goto  continueLabel)
                                 ] ++ fnBody ++ [
                                  SL continueLabel
                                 ]
  return newCallRecordCommands


_compile (MultiApp t1 t2 _) opt = do
  c1 <- _compile t1 opt
  c2s <- mapM (\x -> _compile x opt) t2
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  callRecordIdent <- nextIdentName
  callRecordIdent' <- nextIdentName
  nparams <- nextIdentName
  currentCount <- nextIdentName
  retsFn <- case (getType t1) of
            FnT _ (FnT _ _) -> return True
            FnT _ ListT     -> return True
            FnT _ _         -> return False
            _               -> error $ "Applying something that is not an FnT"
  addParams <- return $ concat $ map (\c-> c ++ [
                                         SC (AssignV "param" "resp"),
                                         SC (IR.Op currentCount currentCount IR.Add "one"),
                                         SC (SetCallRecordParam callRecordIdent' currentCount "param")
                                       ]
                            ) c2s

  bbThen <- return $ [
                       SL thenLabel,
                       SC (AssignV callRecordIdent callRecordIdent'),
                       SC (Call "resp" callRecordIdent retsFn),
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


_compile (App t1 t2 _) opt = do
  c1 <- _compile t1 opt
  c2 <- _compile t2 opt
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  callRecordIdent <- nextIdentName
  callRecordIdent' <- nextIdentName
  retsFn <- case (getType t1) of
            FnT _ (FnT _ _) -> return True
            FnT _ ListT     -> return True
            FnT _ _         -> return False
            _               -> error $ "Applying something that is not an FnT"
  bbThen <- return $ [
                       SL thenLabel,
                       SC (AssignV callRecordIdent callRecordIdent'),
                       SC (Call "resp" callRecordIdent retsFn),
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

_compile (L1.List elements _) _ = do
  listIdent <- nextIdentName
  listSets <- return $ listSetsFun elements listIdent 0
  idents <- return $ listIdents elements
  identCode <- mapM (\a-> (getFromSymbolTable a) ) idents
  identCode' <- return $ (concat (map (\(a,b) -> a ++ [SC (AssignV b "resp")]) (zip identCode idents)))
  return $ identCode' ++ [ SC (NewList listIdent (length elements)) ] ++ listSets ++ [SC (AssignV resp listIdent)]

_compile (L1.Head t1 _) opt = do
  c1 <- _compile t1 opt
  return $ c1 ++ [SC (IR.Head resp resp)]

_compile (L1.Tail t1 _) opt = do
  c1 <- _compile t1 opt
  return $ c1 ++ [SC (IR.Tail resp resp)]

_compile (L1.Last t1 _) opt = do
  c1 <- _compile t1 opt
  return $ c1 ++ [SC (IR.Last resp resp)]

_compile (L1.Concat t1 t2 _) opt = do
  c1 <- _compile t1 opt
  c2 <- _compile t2 opt
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  cs <- return $ t1c ++ t2c ++ [SC (IR.Concat "resp" t1Ident t2Ident)]
  return cs

_compile (L1.Concat3 t1 t2 t3 _) opt = do
  c1 <- _compile t1 opt
  c2 <- _compile t2 opt
  c3 <- _compile t3 opt
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t3Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  t3c <- return $ c3 ++ [SC (AssignV t3Ident resp)]
  cs <- return $ t1c ++ t2c ++ t3c ++ [SC (IR.Concat3 "resp" t1Ident t2Ident t3Ident)]
  return cs

_compile (L1.Slice t1 t2 t3 _) opt = do
  c1 <- _compile t1 opt
  c2 <- _compile t2 opt
  c3 <- _compile t3 opt
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t3Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  t3c <- return $ c3 ++ [SC (AssignV t3Ident resp)]
  cs <- return $ t1c ++ t2c ++ t3c ++ [SC (IR.Slice "resp" t1Ident t2Ident t3Ident)]
  return cs

_compile (L1.Map t1 t2 _) opt = do
  c1 <- _compile t1 opt
  c2 <- _compile t2 opt
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  thenLabel <- nextThenLabel
  thenLabel2 <- nextThenLabel
  thenLabel3 <- nextThenLabel
  _ <- nextBackLabel
  _ <- nextBackLabel
  _ <- nextBackLabel
  dummyLabel <- nextDummyLabel
  dummyLabel2 <- nextDummyLabel
  dummyLabel3 <- nextDummyLabel
  mapCode <- return $ [
      SC (AssignI "two" 2),
      SC (AssignI "three" 3),
      SC (AssignI "four" 4),
      SC (AssignI "six" 6),
      SC (AssignI "eight" 8),
      SC (GetNPU "pus"),
      SC (IR.Length "listSize" t2Ident),
      SC (IR.Op "resp" "pus" IR.Mult "eight"),
      SC (IR.Op "b1" "listSize" IR.Greater "resp"),
      SC (IR.Op "b2" "pus" IR.Greater "eight"),
      SC (IR.Op "resp" "b1" IR.And "b2"),

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
      SC (IR.Op "divisor" "pus" IR.Div "pus"),
      SC (IR.Op "sliceSize" "listSize" IR.Div "divisor"),
      SC (IR.Op "numberOfResults" "listSize" IR.Div "sliceSize"),
      SC (NewListN "partialResultLists" "numberOfResults"),
      SC (IR.AssignI "start" 0),
      SC (IR.AssignV "end" "sliceSize"),
      SC (IR.AssignI "idx" 0),
      SC (IR.AssignI "one" 1),
      ST (Goto thenLabel2),

      SL thenLabel2,
      SC (IR.Slice "list" t2Ident  "start" "end"),
      SC (IR.Length "thisSliceSize" "list"),
      SC (NewCallRecord "callRecord" 3),
      SC (SetCallRecordFn "callRecord" "splitMap"),
      SC (SetCallRecordMissingI "callRecord" 0),
      SC (SetCallRecordCountI "callRecord" 3),
      SC (SetCallRecordParamI "callRecord" 0 t1Ident),
      SC (SetCallRecordParamI "callRecord" 1 "list"),
      SC (SetCallRecordParamI "callRecord" 2 "thisSliceSize"),
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
  cs <- return $ if opt == 0 || opt == 1
                 then t1c ++ t2c ++ mapCode
                 else t1c ++ t2c ++ [SC (IR.Map "resp" t1Ident t2Ident), SC Wait] 
  return cs

_compile (L1.Filter t1 t2 _) opt = do
  c1 <- _compile t1 opt
  c2 <- _compile t2 opt
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t3Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  cs <- return $ t1c ++ t2c ++ [SC (IR.Map t3Ident t1Ident t2Ident), SC Wait, SC (IR.Filter "resp" t3Ident t2Ident)]
  return cs

_compile (L1.Length t1 _) opt = do
  c1 <- _compile t1 opt
  return $ c1 ++ [SC (IR.Length resp resp)]

_compile (L1.Op t1 op t2 _) opt = do
  c1 <- _compile t1 opt
  c2 <- _compile t2 opt
  t1Ident <- nextIdentName
  t2Ident <- nextIdentName
  t1c <- return $ c1 ++ [SC (AssignV t1Ident resp)]
  t2c <- return $ c2 ++ [SC (AssignV t2Ident resp)]
  cs <- return $ t1c ++ t2c ++ [SC (IR.Op "resp" t1Ident (setOp op) t2Ident)]
  return cs

_compile (L1.If t1 t2 t3 _) opt = do
  c1 <- _compile t1 opt
  c2 <- _compile t2 opt
  c3 <- _compile t3 opt
  thenLabel <- nextThenLabel
  backLabel <- nextBackLabel
  dummyLabel <- nextDummyLabel
  continueLabel <- nextContinueLabel
  t1c <- return $ c1 ++ [ST (IR.If resp thenLabel)] ++ [SL dummyLabel]
  t3c <- return $ c3 ++ [ST (Goto backLabel)] ++ [SL backLabel]
  bbThen <- return $ [SL thenLabel] ++ c2 ++ [ST (Goto backLabel)]
  cs <- return $ t1c ++ t3c ++ [ST (Goto continueLabel)] ++ bbThen ++ [(SL continueLabel)]
  return cs

_compile (Let n (Fn params t1 (typ,freeVars)) t2 _) opt = do
  callRecordIdent <- nextIdentName
  FnT paramTypes _ <- return typ
  _ <- forM [0..((length params)-1)] $ \i ->
    setSymbolTable (params!!i) $
                                 if (paramTypes!!i) == IntT
                                 then [SC (AssignV "resp" (params!!i))]
                                 else [SC (AssignV (params!!i) (params!!i)), SC (InnerCopy "resp" (params!!i))]
  innerCopyTempId <- nextIdentName
  freeVarsSetParams <- return $ concat $ map (\((fv,typ'),i)-> if typ' == IntT || fv  == "arg"
                                                               then [SC (SetCallRecordParamI callRecordIdent i fv)]
                                                               else [SC (InnerCopy innerCopyTempId fv), SC (SetCallRecordParamI callRecordIdent i innerCopyTempId)]
                                             ) (zip freeVars [0..])
  fn <- nextFnLabel
  newCallRecordStruct <- return $ [
                                    SC (NewCallRecord callRecordIdent ((length freeVars)+(length params))),
                                    SC (SetCallRecordFn callRecordIdent fn)
                                  ] ++ freeVarsSetParams ++ [
                                    SC (SetCallRecordMissingI callRecordIdent (length params)),
                                    SC (SetCallRecordCountI callRecordIdent (length freeVars)),
                                    SC (AssignV resp callRecordIdent)
                                  ]
  _ <- setSymbolTable n newCallRecordStruct
  c1 <- _compile t1 opt
  getParamsCommands <- return $ map (\((fv,_),i) -> SC (GetCallRecordParam "callRecord" i fv)) (zip freeVars [0..])
  getParamsCommands' <- return $ getParamsCommands ++ (map (\p-> SC (GetCallRecordParam "callRecord" (snd p) (fst p))) (zip params [(length getParamsCommands)..] ))
  refParamsIdxs <- return $ filter (\i -> (paramTypes!!i) /= IntT && ((params!!i) /= "arg")) [0..((length paramTypes)-1)]
  outerCopyCmds <- return $ map (\a-> SC (OuterCopy a a)) $ map (\i->params!!i) refParamsIdxs
  outerCopyCmds' <- return $ outerCopyCmds ++ (map (\a-> SC (OuterCopy a a)) (map (\(a,_) -> a) (filter (\(p,t) -> t /= IntT && (p /= "arg")) freeVars)))
  deleteCmds <- return $ map (\a-> SC (Delete a )) (map (\i->params!!i) refParamsIdxs)
  fnBody <- return $ [SL fn] ++ getParamsCommands' ++ outerCopyCmds' ++ c1 ++ deleteCmds ++ [ST (Return resp)]
  continueLabel <- nextContinueLabel
  newCallRecordCommands <- return $  [
                                  ST (Goto  continueLabel)
                                 ] ++ fnBody ++ [
                                  SL continueLabel
                                 ]
  c2 <- _compile t2 opt
  cs <- return $ c2
  return $ newCallRecordCommands ++ cs

_compile (Let n (L1.List elements _) t2 _) opt = do
  listIdent <- nextIdentName
  listSets <- return $ listSetsFun elements listIdent 0
  idents <- return $ listIdents elements
  identCode <- mapM (\a-> (getFromSymbolTable a) ) idents
  identCode' <- return $ (concat (map (\(a,b) -> a ++ [SC (AssignV b "resp")]) (zip identCode idents)))
  _ <- setSymbolTable n $ identCode' ++ [ SC (NewList listIdent (length elements)) ] ++ listSets ++ [SC (AssignV resp listIdent)]
  c2 <- _compile t2 opt
  return $ c2

_compile (Let n (Num i _) t2 _) opt = do
  _ <- setSymbolTable n [SC (AssignI "resp" i)]
  c2 <- _compile t2 opt
  return $ c2

_compile (Let n t1 t2 _) opt = do
  c1 <- _compile t1 opt
  tableCmds <- return $ if (getType t1) == IntT
                        then [SC (AssignV "resp" n)]
                        else [SC (InnerCopy "resp" n)]
  _ <- setSymbolTable n tableCmds
  c2 <- _compile t2 opt
  cs <- return $ c1 ++ [SC (AssignV n "resp")] ++ c2
  return cs


_compile (Letrec n (Fn params t1 (typ,freeVars)) t2 _) opt = do
  callRecordIdent <- nextIdentName
  _ <- forM [0..((length params)-1)] $ \i ->
    setSymbolTable (params!!i) $ case typ of
                                 FnT paramTypes _ -> if (paramTypes!!i) == IntT
                                                     then [SC (AssignV "resp" (params!!i))]
                                                     else [SC (AssignV (params!!i) (params!!i)), SC (InnerCopy "resp" (params!!i))]
                                 _                -> error $ "Applying function that don't have type FnT: " ++ (show typ)
  innerCopyTempId <- nextIdentName
  FnT paramTypes _ <- return typ
  freeVarsSetParams <- return $ concat $ map (\((fv,typ'),i)-> if typ' == IntT || fv  == "arg"
                                                               then [SC (SetCallRecordParamI callRecordIdent i fv)]
                                                               else [SC (InnerCopy innerCopyTempId fv), SC (SetCallRecordParamI callRecordIdent i innerCopyTempId)]
                                             ) (zip freeVars [0..])
  fn <- nextFnLabel
  newCallRecordStruct <- return $ [
                                    SC (NewCallRecord callRecordIdent ((length freeVars)+(length params))),
                                    SC (SetCallRecordFn callRecordIdent fn)
                                  ] ++ freeVarsSetParams ++ [
                                    SC (SetCallRecordMissingI callRecordIdent (length params)),
                                    SC (SetCallRecordCountI callRecordIdent (length freeVars)),
                                    SC (AssignV resp callRecordIdent)
                                  ]
  _ <- setSymbolTable n newCallRecordStruct
  c1 <- _compile t1 opt

  refParamsIdxs <- return $ filter (\i -> (paramTypes!!i) /= IntT && ((params!!i) /= "arg")) [0..((length paramTypes)-1)]
  outerCopyCmds <- return $ map (\a-> SC (OuterCopy a a)) $ map (\i->params!!i) refParamsIdxs
  outerCopyCmds' <- return $ outerCopyCmds ++ (map (\a-> SC (OuterCopy a a)) (map (\(a,_) -> a) (filter (\(p,t) -> t /= IntT && (p /= "arg")) freeVars)))
  deleteCmds <- return $ map (\a-> SC (Delete a )) (map (\i->params!!i) refParamsIdxs)
  getParamsCommands <- return $ map (\((fv,_),i) -> SC (GetCallRecordParam "callRecord" i fv)) (zip freeVars [0..])
  getParamsCommands' <- return $ getParamsCommands ++ (map (\p-> SC (GetCallRecordParam "callRecord" (snd p) (fst p))) (zip params [(length getParamsCommands)..] ))
  fnBody <- return $ [SL fn] ++ getParamsCommands' ++ outerCopyCmds' ++ c1 ++ deleteCmds ++ [ST (Return resp)]
  continueLabel <- nextContinueLabel

  c2 <- _compile t2 opt
  cs <- return $ [ ST (Goto  continueLabel) ] ++ fnBody ++ [ SL continueLabel ] ++ c2
  return $ cs

_compile (Letrec _ _ _ _) _ = error "Letrec first term should be a function"


listIdents :: [ListItem] -> [String]
listIdents elements = concat $ map (\e-> case e of
                                             ListNum _ -> []
                                             ListIdent n -> [n]
                                             RecList l -> listIdents l
                                       ) elements

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
      GetCallRecordParam "callRecord" 2 "size",
      AssignV "fn" "fn",
      -- AssignV "list" "list",
      IR.SMap "resp" "fn" "list" "size",
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
setOp L1.Mod = IR.Mod
setOp L1.LesserEqual = IR.LesserEqual
