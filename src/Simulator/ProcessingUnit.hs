module Simulator.ProcessingUnit where

import qualified Data.Map as Map

import AcquaIR.Language
import Simulator.Environment
import Simulator.Interconnection

data ReturnAddr = ReturnAddr {
  addr_pId :: PId,
  addr_envId :: EnvId,
  variable :: Name
} deriving (Show,Eq)

data ExecutionContext = ExecutionContext {
  ec_c :: [Command],
  ec_t :: Terminator
} deriving (Show,Eq)

data ProcessingUnit = PU {
  puId :: PId,

  commands :: [Command],
  terminator :: Terminator,

  currentEnv :: EnvId,
  environments :: Map.Map EnvId Environment,

  returnAddrs :: Map.Map EnvId ReturnAddr,
  callCount :: Map.Map EnvId Int,
  sleepingExecution :: Map.Map EnvId ExecutionContext,
  outgoingMessageQueue :: Interconnection,

  enabled :: Bool,
  locked :: Bool
} deriving (Show,Eq)

specialPU :: ProcessingUnit
specialPU = PU 0 [] Empty
                 "0" (Map.fromList [("0",emptyEnv)]) (Map.fromList [])
                 (Map.fromList [("0",1)]) (Map.fromList [("0", ExecutionContext [] Empty)]) [] False False

newPU :: Int -> ProcessingUnit
newPU n = PU n [] Empty
               "" (Map.fromList []) (Map.fromList [])
               (Map.fromList []) (Map.fromList []) [] False False

newProcessingUnits :: Int -> [ProcessingUnit]
newProcessingUnits n = specialPU : (map newPU [1..n])

unlock :: ProcessingUnit -> ProcessingUnit
unlock (PU pId c t ce env ra cc se omq enbl _)
      = (PU pId c t ce env ra cc se omq enbl False)

canExecuteCmds :: ProcessingUnit -> Bool
canExecuteCmds pu = (enabled pu) && (not (locked pu))
