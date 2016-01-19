module Simulator.ProcessingUnit where

import qualified Data.Map as Map

import AcquaIR.Language
import Simulator.Environment

type PId = Int

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
  runEnvs :: Map.Map EnvId Environment,
  copyEnvs :: Map.Map EnvId Environment,

  returnAddrs :: Map.Map EnvId ReturnAddr,
  callCount :: Map.Map EnvId Int,
  sleepingExecution :: Map.Map EnvId ExecutionContext,

  tainted :: Bool
} deriving (Show,Eq)

specialPU :: ProcessingUnit
specialPU = PU 0 [] Empty
                 "0" (Map.fromList [("0",emptyEnv)]) (Map.fromList [("0",emptyEnv)])
                 (Map.fromList []) (Map.fromList [("0",1)]) (Map.fromList []) False

newPU :: Int -> ProcessingUnit
newPU n = PU n [] Empty
               "" (Map.fromList []) (Map.fromList [])
               (Map.fromList []) (Map.fromList []) (Map.fromList []) False

newProcessingUnits :: Int -> [ProcessingUnit]
newProcessingUnits n = specialPU : (map newPU [1..n])

untaint :: ProcessingUnit -> ProcessingUnit
untaint (PU pId c t ce rEnv cEnv ra cc se _)
      = (PU pId c t ce rEnv cEnv ra cc se False)
