module Compiler.CompileStates where

import Logger
import AcquaIR.Language as IR
import Control.Monad.State


type ClosureInfoT = (String,Int,Bool)

data CompileStates = CompileStates {
  thenLabelNum :: Int,
  backLabelNum :: Int,
  dummyLabelNum :: Int,
  fnLabelNum :: Int,
  identNum :: Int,
  closureInfo :: [(String,ClosureInfoT)],
  fnLabelNames :: [(String,String)]
  }

defaultCompileStates :: CompileStates
defaultCompileStates = (CompileStates 0 0 0 0 0 [] [])

nextThenLabel :: State CompileStates Label
nextThenLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels <- get
  put (CompileStates (thenN+1) backN dummyN fnN identN closInfo fnLabels)
  return $ "then" ++ (show thenN)

nextBackLabel :: State CompileStates Label
nextBackLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels <- get
  put (CompileStates thenN (backN+1) dummyN fnN identN closInfo fnLabels)
  return $ "back" ++ (show backN)

nextDummyLabel :: State CompileStates Label
nextDummyLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels <- get
  put (CompileStates thenN backN (dummyN+1) fnN identN closInfo fnLabels)
  return $ "dummy" ++ (show dummyN)

nextFnLabel :: State CompileStates Label
nextFnLabel = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels <- get
  put (CompileStates thenN backN dummyN (fnN+1) identN closInfo fnLabels)
  return $ "_fn_" ++ (show fnN)

nextIdentName :: State CompileStates IR.Name
nextIdentName = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels <- get
  put (CompileStates thenN backN dummyN fnN (identN+1) closInfo fnLabels)
  return $ "var" ++ (show identN)

setClosureInfo :: String -> ClosureInfoT -> State CompileStates IR.Name
setClosureInfo a b = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels <- get
  closInfo' <- return $ (a,b) : (closInfo)
  put (CompileStates thenN backN dummyN fnN identN closInfo' fnLabels)
  return $ "var" ++ (show identN)

getClosureInfo :: String -> State CompileStates (Maybe ClosureInfoT)
getClosureInfo a = do
  s <- get
  return $ lookup a (closureInfo s)

setFnLabelName :: String -> String -> State CompileStates IR.Name
setFnLabelName a b = do
  CompileStates thenN backN dummyN fnN identN closInfo fnLabels <- get
  fnLabels' <- return $ (a,b) : fnLabels
  put (CompileStates thenN backN dummyN fnN identN closInfo fnLabels')
  return $ "var" ++ (show identN)

getFnLabelName :: String -> State CompileStates String
getFnLabelName a = do
  s <- get
  return $ case lookup a (fnLabelNames s) of
           Just ret -> ret
           Nothing -> traceShow (a, fnLabelNames s) $ error "Can't find symbol on getFnLabelName!"

