module AcquaIR.Compile where

import Debug.Trace

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
  fnLabelNum :: Int,
  identNum :: Int,
  fnVarNames :: [(String,String)],
  fnLabelNames :: [(String,String)]
  }

defaultStates :: States
defaultStates = (States 0 0 0 0 0 [] [])

resp :: IR.Name
resp = "resp"

compile :: L1.Term -> IR.Program
compile t =
  addWaits (eliminateCallNextVar (statementsToProgram statements))
  where
    (c, bb) = evalState (_compile t) defaultStates
    statements = [SL "main"] ++ c ++ [ST (Return resp)] ++ bb

_compile :: L1.Term -> State States ([Statement], [Statement])
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
  t2c <- return $ c2 ++ [SC (AssignV paramName resp)] 
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
  (Fn varName _) <- return t1
  _ <- setFnVarName n varName
  (c1,bb1) <- _compile t1
  (SL fn_name) <- return $ head bb1
  (c2,bb2) <- _compile t2
  cs <- return $  [SC (AssignL n fn_name)] ++ c1 ++ c2
  return (cs, bb1 ++ bb2)


-- TODO: Clean up next state functions
nextThenLabel :: State States Label
nextThenLabel = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  fnN <- return (fnLabelNum s)
  identN <- return (identNum s)
  fnNames <- return (fnVarNames s)
  fnLabels <- return (fnLabelNames s)
  put (States (thenN+1) backN dummyN fnN identN fnNames fnLabels)
  return $ "then" ++ (show thenN)

nextBackLabel :: State States Label
nextBackLabel = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  fnN <- return (fnLabelNum s)
  identN <- return (identNum s)
  fnNames <- return (fnVarNames s)
  fnLabels <- return (fnLabelNames s)
  put (States thenN (backN+1) dummyN fnN identN fnNames fnLabels)
  return $ "back" ++ (show backN)

nextDummyLabel :: State States Label
nextDummyLabel = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  fnN <- return (fnLabelNum s)
  identN <- return (identNum s)
  fnNames <- return (fnVarNames s)
  fnLabels <- return (fnLabelNames s)
  put (States thenN backN (dummyN+1) fnN identN fnNames fnLabels)
  return $ "dummy" ++ (show dummyN)

nextFnLabel :: State States Label
nextFnLabel = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  fnN <- return (fnLabelNum s)
  identN <- return (identNum s)
  fnNames <- return (fnVarNames s)
  fnLabels <- return (fnLabelNames s)
  put (States thenN backN dummyN (fnN+1) identN fnNames fnLabels)
  return $ "_fn_" ++ (show fnN)

nextIdentName :: State States IR.Name
nextIdentName = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  fnN <- return (fnLabelNum s)
  identN <- return (identNum s)
  fnNames <- return (fnVarNames s)
  fnLabels <- return (fnLabelNames s)
  put (States thenN backN dummyN fnN (identN+1) fnNames fnLabels)
  return $ "var" ++ (show identN)

setFnVarName :: String -> String -> State States IR.Name
setFnVarName a b = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  fnN <- return (fnLabelNum s)
  identN <- return (identNum s)
  fnNames' <- (return ((a,b) : (fnVarNames s)))
  fnLabels <- return (fnLabelNames s)
  put (States thenN backN dummyN fnN identN fnNames' fnLabels)
  return $ "var" ++ (show identN)

getFnVarName :: String -> State States String
getFnVarName a = do
  s <- get
  return $ case lookup a (fnVarNames s) of
           Just ret -> ret
           Nothing -> traceStack (show (a, fnVarNames s)) $ error "Can't find symbol!"

setFnLabelName :: String -> String -> State States IR.Name
setFnLabelName a b = do
  s <- get
  thenN <- return (thenLabelNum s)
  backN <- return (backLabelNum s)
  dummyN <- return (dummyLabelNum s)
  fnN <- return (fnLabelNum s)
  identN <- return (identNum s)
  fnNames <- return (fnVarNames s)
  fnLabels' <- (return ((a,b) : (fnLabelNames s)))
  put (States thenN backN dummyN fnN identN fnNames fnLabels')
  return $ "var" ++ (show identN)

getFnLabelName :: String -> State States String
getFnLabelName a = do
  s <- get
  return $ case lookup a (fnLabelNames s) of
           Just ret -> ret
           Nothing -> traceShow (a, fnLabelNames s) $ error "Can't find symbol!"



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



-- Statements to program

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


eliminateCallNextVar :: IR.Program -> IR.Program
eliminateCallNextVar [] = []
eliminateCallNextVar (bb:bbs) =
  bb':(eliminateCallNextVar bbs)
  where
    BB l n cs t = bb
    bb' = BB l n (eliminateCallNextVar' cs) t
    eliminateCallNextVar' [] = []
    eliminateCallNextVar' (c:cs) = case (c,cs) of
                                        ((Call ret l env),(AssignV x1 x2):cs') ->
                                              (Call x1 l env):(eliminateCallNextVar' cs')
                                        (_,_) -> c:(eliminateCallNextVar' cs)
    eliminateCallNextVar' (c:cs) = c : (eliminateCallNextVar' cs)



addWaits :: IR.Program -> IR.Program
addWaits [] = []
addWaits (bb:bbs) =
  bb':(addWaits bbs)
  where
    BB l n' c t = bb
    bb' = BB l n' (addWaits' c [])  t
    addWaits' [] vars  = if null vars then [] else [Wait]
    addWaits' (c':cs') vars = case c' of
                           Call ret _ _ -> c':(addWaits' cs' (ret:vars))
                           AssignI n _ -> if elem n vars
                                            then Wait:c':(addWaits' cs' [])
                                            else c':(addWaits' cs' vars)
                           AssignL n _ -> if elem n vars
                                            then Wait:c':(addWaits' cs' [])
                                            else c':(addWaits' cs' vars)
                           AssignV n1 n2 -> if elem n1 vars || elem n2 vars
                                            then Wait:c':(addWaits' cs' [])
                                            else c':(addWaits' cs' vars)
                           IR.Op n1 _ n2 -> if elem n1 vars || elem n2 vars
                                            then Wait:c':(addWaits' cs' [])
                                            else c':(addWaits' cs' vars)
                           _ -> c':(addWaits' cs' vars)

