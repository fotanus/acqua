module AcquaIR.CompileStates where

import Debug.Trace
import AcquaIR.Language as IR
import Control.Monad.State

data CompileStates = CompileStates {
  thenLabelNum :: Int,
  backLabelNum :: Int,
  dummyLabelNum :: Int,
  fnLabelNum :: Int,
  identNum :: Int,
  fnVarNames :: [(String,String)],
  fnLabelNames :: [(String,String)]
  }

defaultCompileStates :: CompileStates
defaultCompileStates = (CompileStates 0 0 0 0 0 [] [])

nextThenLabel :: State CompileStates Label
nextThenLabel = do
  CompileStates thenN backN dummyN fnN identN fnNames fnLabels <- get
  put (CompileStates (thenN+1) backN dummyN fnN identN fnNames fnLabels)
  return $ "then" ++ (show thenN)

nextBackLabel :: State CompileStates Label
nextBackLabel = do
  CompileStates thenN backN dummyN fnN identN fnNames fnLabels <- get
  put (CompileStates thenN (backN+1) dummyN fnN identN fnNames fnLabels)
  return $ "back" ++ (show backN)

nextDummyLabel :: State CompileStates Label
nextDummyLabel = do
  CompileStates thenN backN dummyN fnN identN fnNames fnLabels <- get
  put (CompileStates thenN backN (dummyN+1) fnN identN fnNames fnLabels)
  return $ "dummy" ++ (show dummyN)

nextFnLabel :: State CompileStates Label
nextFnLabel = do
  CompileStates thenN backN dummyN fnN identN fnNames fnLabels <- get
  put (CompileStates thenN backN dummyN (fnN+1) identN fnNames fnLabels)
  return $ "_fn_" ++ (show fnN)

nextIdentName :: State CompileStates IR.Name
nextIdentName = do
  CompileStates thenN backN dummyN fnN identN fnNames fnLabels <- get
  put (CompileStates thenN backN dummyN fnN (identN+1) fnNames fnLabels)
  return $ "var" ++ (show identN)

setFnVarName :: String -> String -> State CompileStates IR.Name
setFnVarName a b = do
  CompileStates thenN backN dummyN fnN identN fnNames fnLabels <- get
  fnNames' <- return $ (a,b) : (fnNames)
  put (CompileStates thenN backN dummyN fnN identN fnNames' fnLabels)
  return $ "var" ++ (show identN)

getFnVarName :: String -> State CompileStates String
getFnVarName a = do
  s <- get
  return $ case lookup a (fnVarNames s) of
           Just ret -> ret
           Nothing -> traceShow (a, fnVarNames s) $ error "Can't find symbol!"

setFnLabelName :: String -> String -> State CompileStates IR.Name
setFnLabelName a b = do
  CompileStates thenN backN dummyN fnN identN fnNames fnLabels <- get
  fnLabels' <- return $ (a,b) : fnLabels
  put (CompileStates thenN backN dummyN fnN identN fnNames fnLabels')
  return $ "var" ++ (show identN)

getFnLabelName :: String -> State CompileStates String
getFnLabelName a = do
  s <- get
  return $ case lookup a (fnLabelNames s) of
           Just ret -> ret
           Nothing -> traceShow (a, fnLabelNames s) $ error "Can't find symbol!"

