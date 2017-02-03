module Compiler.CompileStates where

import Logger
import AcquaIR.Language as IR
import Compiler.CompileStatement
import Control.Monad.State


data CompileStates = CompileStates {
  thenLabelNum :: Int,
  backLabelNum :: Int,
  dummyLabelNum :: Int,
  fnLabelNum :: Int,
  identNum :: Int,
  symbolTable :: [(String, [Statement])],
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

getFromSymbolTable :: String -> State CompileStates [Statement]
getFromSymbolTable a = do
  s <- get
  return $ case lookup a (symbolTable s) of
                Just something -> something
                Nothing -> [SC (AssignV "resp" a)]

setSymbolTable :: String -> [Statement] -> State CompileStates String
setSymbolTable a b = do
  CompileStates thenN backN dummyN fnN identN symbols fnLabels kvs continueN <- get
  symbols' <- return $ (a,b) : (symbols)
  put (CompileStates thenN backN dummyN fnN identN symbols' fnLabels kvs continueN)
  return $ "nothing"

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

