module Compiler.CompileStates where

import Logger
import AcquaIR.Language as IR
import Control.Monad.State


type CallRecordInfoT = (String,String,[String],[String],Bool)

data CompileStates = CompileStates {
  thenLabelNum :: Int,
  backLabelNum :: Int,
  dummyLabelNum :: Int,
  fnLabelNum :: Int,
  identNum :: Int,
  callRecordInfo :: [(String,CallRecordInfoT)],
  fnLabelNames :: [(String,String)],
  knownVarsList :: [String],
  continueLabelNum :: Int
  }

defaultCompileStates :: CompileStates
defaultCompileStates = (CompileStates 0 0 0 0 0 [] [] [] 0)

nextContinueLabel :: State CompileStates Label
nextContinueLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs continueN <- get
  put (CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs (continueN + 1))
  return $ "continue" ++ (show continueN)

nextThenLabel :: State CompileStates Label
nextThenLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs continueN <- get
  put (CompileStates (thenN+1) backN dummyN fnN identN closInfo fnLabels kvs continueN)
  return $ "then" ++ (show thenN)

nextBackLabel :: State CompileStates Label
nextBackLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs continueN <- get
  put (CompileStates thenN (backN+1) dummyN fnN identN closInfo fnLabels kvs continueN)
  return $ "back" ++ (show backN)

nextDummyLabel :: State CompileStates Label
nextDummyLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs continueN <- get
  put (CompileStates thenN backN (dummyN+1) fnN identN closInfo fnLabels kvs continueN)
  return $ "dummy" ++ (show dummyN)

lastFnLabel :: State CompileStates Label
lastFnLabel = do
  CompileStates _ _ _ fnN _ _ _ _ _ <- get
  return $ "_fn_" ++ (show (fnN-1))

nextFnLabel :: State CompileStates Label
nextFnLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs continueN <- get
  put (CompileStates thenN backN dummyN (fnN+1) identN closInfo fnLabels kvs continueN)
  return $ "_fn_" ++ (show fnN)

nextIdentName :: State CompileStates IR.Name
nextIdentName = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs continueN <- get
  put (CompileStates thenN backN dummyN fnN (identN+1) closInfo fnLabels kvs continueN)
  return $ "var" ++ (show identN)

setCallRecordInfo :: String -> CallRecordInfoT -> State CompileStates IR.Name
setCallRecordInfo a b = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs continueN <- get
  closInfo' <- return $ (a,b) : (closInfo)
  put (CompileStates thenN backN dummyN fnN identN closInfo' fnLabels kvs continueN)
  return $ "var" ++ (show identN)

getCallRecordInfo :: String -> State CompileStates (Maybe CallRecordInfoT)
getCallRecordInfo a = do
  s <- get
  return $ lookup a (callRecordInfo s)

setFnLabelName :: String -> String -> State CompileStates IR.Name
setFnLabelName a b = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs continueN <- get
  fnLabels' <- return $ (a,b) : fnLabels
  put (CompileStates thenN backN dummyN fnN identN closInfo fnLabels' kvs continueN)
  return $ "var" ++ (show identN)

addKnownVars :: String -> State CompileStates IR.Name
addKnownVars a = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs continueN <- get
  kvs' <- return $ a : kvs
  put (CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs' continueN)
  return $ "var" ++ (show identN)

getKnownVars :: State CompileStates [String]
getKnownVars = do
  s <- get
  return $ knownVarsList s

getFnLabelName :: String -> State CompileStates String
getFnLabelName a = do
  s <- get
  return $ case lookup a (fnLabelNames s) of
           Just ret -> ret
           Nothing -> traceShow (a, fnLabelNames s) $ error "Can't find symbol on getFnLabelName!"

