module Compiler.CompileStates where

import Logger
import AcquaIR.Language as IR
import Control.Monad.State


type ClosureInfoT = (String,String,[String],Bool)

data CompileStates = CompileStates {
  thenLabelNum :: Int,
  backLabelNum :: Int,
  dummyLabelNum :: Int,
  fnLabelNum :: Int,
  identNum :: Int,
  closureInfo :: [(String,ClosureInfoT)],
  fnLabelNames :: [(String,String)],
  knownVarsList :: [String]
  }

defaultCompileStates :: CompileStates
defaultCompileStates = (CompileStates 0 0 0 0 0 [] [] [])

nextThenLabel :: State CompileStates Label
nextThenLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs <- get
  put (CompileStates (thenN+1) backN dummyN fnN identN closInfo fnLabels kvs)
  return $ "then" ++ (show thenN)

nextBackLabel :: State CompileStates Label
nextBackLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs <- get
  put (CompileStates thenN (backN+1) dummyN fnN identN closInfo fnLabels kvs)
  return $ "back" ++ (show backN)

nextDummyLabel :: State CompileStates Label
nextDummyLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs <- get
  put (CompileStates thenN backN (dummyN+1) fnN identN closInfo fnLabels kvs)
  return $ "dummy" ++ (show dummyN)

nextFnLabel :: State CompileStates Label
nextFnLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs <- get
  put (CompileStates thenN backN dummyN (fnN+1) identN closInfo fnLabels kvs)
  return $ "_fn_" ++ (show fnN)

nextIdentName :: State CompileStates IR.Name
nextIdentName = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs <- get
  put (CompileStates thenN backN dummyN fnN (identN+1) closInfo fnLabels kvs)
  return $ "var" ++ (show identN)

setClosureInfo :: String -> ClosureInfoT -> State CompileStates IR.Name
setClosureInfo a b = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs <- get
  closInfo' <- return $ (a,b) : (closInfo)
  put (CompileStates thenN backN dummyN fnN identN closInfo' fnLabels kvs)
  return $ "var" ++ (show identN)

getClosureInfo :: String -> State CompileStates (Maybe ClosureInfoT)
getClosureInfo a = do
  s <- get
  return $ lookup a (closureInfo s)

setFnLabelName :: String -> String -> State CompileStates IR.Name
setFnLabelName a b = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs <- get
  fnLabels' <- return $ (a,b) : fnLabels
  put (CompileStates thenN backN dummyN fnN identN closInfo fnLabels' kvs)
  return $ "var" ++ (show identN)

addKnownVars :: String -> State CompileStates IR.Name
addKnownVars a = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs <- get
  kvs' <- return $ a : kvs
  put (CompileStates thenN backN dummyN fnN identN closInfo fnLabels kvs')
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

