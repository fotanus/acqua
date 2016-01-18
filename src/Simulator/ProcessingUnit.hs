module Simulator.ProcessingUnit where

import qualified Data.Map as Map

import AcquaIR.Language
import Simulator.Environment

type PId = Int

data ReturnAddr = ReturnAddr {
  addr_pId :: PId,
  addr_envId :: EnvId,
  x :: Name
}

data ExecutionContext = ExecutionContext {
  ec_c :: [Command],
  ec_t :: Terminator
}

data ProcessingUnit = PU {
  pId :: PId,

  c :: [Command],
  t :: Terminator,

  currentEnv :: EnvId,
  runEnvs :: Map.Map EnvId Environment,
  copyEnvs :: Map.Map EnvId Environment,

  returnAddrs :: Map.Map EnvId ReturnAddr,
  callCount :: Map.Map EnvId Int,
  sleepingExecution :: Map.Map EnvId ExecutionContext
}

specialPU :: ProcessingUnit
specialPU = PU 0 [] Empty
                 "0" (Map.fromList [("0",emptyEnv)]) (Map.fromList [])
                 (Map.fromList []) (Map.fromList [("0",1)]) (Map.fromList [])

newPU :: Int -> ProcessingUnit
newPU n = PU n [] Empty
               "" (Map.fromList []) (Map.fromList [])
               (Map.fromList []) (Map.fromList []) (Map.fromList [])

newProcessingUnits n = specialPU : (map newPU [1..n])
