module Simulator.ProcessingUnit where

import qualified Data.Map as Map
import qualified Data.List as List

import AcquaIR.Language
import Simulator.Value
import Simulator.Environment
import Simulator.Heap
import Simulator.Interconnection
import Simulator.CallRecord
import Simulator.ProcessingUnitId

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
  heap :: Heap,
  returnAddrs :: Map.Map EnvId ReturnAddr,
  callCount :: Map.Map EnvId Int,
  sleepingExecution :: Map.Map EnvId ExecutionContext,
  outgoingMessageQueue :: [Message],
  enabled :: Bool,
  locked :: Bool
} deriving (Show,Eq)

emptySpecialPU :: ProcessingUnit
emptySpecialPU =
  let
    environmentZero = Map.fromList [("0", PointerV (Pointer 0 0))]
  in
    PU {
        Simulator.ProcessingUnit.puId=0,
        Simulator.ProcessingUnit.commands = [],
        Simulator.ProcessingUnit.terminator = Empty,
        Simulator.ProcessingUnit.currentEnv = "0",
        Simulator.ProcessingUnit.environments = Map.fromList [("0", environmentZero)],
        Simulator.ProcessingUnit.heap = Map.fromList [(0, CallRecordV emptyCallRecord)],
        Simulator.ProcessingUnit.returnAddrs = Map.fromList [],
        Simulator.ProcessingUnit.callCount = Map.fromList [("0",1)],
        Simulator.ProcessingUnit.sleepingExecution = Map.fromList [("0", ExecutionContext [] Empty)],
        Simulator.ProcessingUnit.outgoingMessageQueue = [],
        Simulator.ProcessingUnit.enabled = False,
        Simulator.ProcessingUnit.locked = False
    }

specialPU :: [Int] -> ProcessingUnit
specialPU pars =
  let
    env = Map.fromList (List.map (\i -> ((show i), PointerV (Pointer 0 i))) [0..(length pars)])
    envs = Map.fromList [("0", env)]
    hp = Map.fromList (List.map (\a -> ((snd a), CallRecordV (callRecordWithParam (fst a)))) (zip pars [0..]))
  in
    PU {
        Simulator.ProcessingUnit.puId=0,
        Simulator.ProcessingUnit.commands = [],
        Simulator.ProcessingUnit.terminator = Empty,
        Simulator.ProcessingUnit.currentEnv = "0",
        Simulator.ProcessingUnit.environments = envs,
        Simulator.ProcessingUnit.heap = hp,
        Simulator.ProcessingUnit.returnAddrs = Map.fromList [],
        Simulator.ProcessingUnit.callCount = Map.fromList [("0",(length pars))],
        Simulator.ProcessingUnit.sleepingExecution = Map.fromList [("0", ExecutionContext [] Empty)],
        Simulator.ProcessingUnit.outgoingMessageQueue = [],
        Simulator.ProcessingUnit.enabled = False,
        Simulator.ProcessingUnit.locked = False
    }

newPU :: Int -> ProcessingUnit
newPU n =
    PU {
        Simulator.ProcessingUnit.puId=n,
        Simulator.ProcessingUnit.commands = [],
        Simulator.ProcessingUnit.terminator = Empty,
        Simulator.ProcessingUnit.currentEnv = "",
        Simulator.ProcessingUnit.environments = Map.fromList [],
        Simulator.ProcessingUnit.heap = Map.fromList [],
        Simulator.ProcessingUnit.returnAddrs = Map.fromList [],
        Simulator.ProcessingUnit.callCount = Map.fromList [],
        Simulator.ProcessingUnit.sleepingExecution = Map.fromList [],
        Simulator.ProcessingUnit.outgoingMessageQueue = [],
        Simulator.ProcessingUnit.enabled = False,
        Simulator.ProcessingUnit.locked = False
    }

getVal :: ProcessingUnit -> Name -> Value
getVal p n =
  let
    Just currEnv = Map.lookup (currentEnv p) (environments p)
    Just val = Map.lookup n currEnv
  in
    val

setVal :: ProcessingUnit -> Name -> Value -> ProcessingUnit
setVal p n v =
  let
    Just currEnv = Map.lookup (currentEnv p) (environments p)
    currEnv' = Map.insert n v currEnv
    envs' = Map.insert (currentEnv p) currEnv' (environments p)
  in
    p { environments = envs' } 

newProcessingUnits :: Int -> [ProcessingUnit]
newProcessingUnits n = (map newPU [1..n])

unlock :: ProcessingUnit -> ProcessingUnit
unlock pu = pu { locked = False }

canExecuteCmds :: ProcessingUnit -> Bool
canExecuteCmds pu = (enabled pu) && (not (locked pu))

currentPuEnv :: ProcessingUnit -> Environment
currentPuEnv pu =
  let
    Just cenv = Map.lookup (currentEnv pu) (environments pu)
  in
    cenv

acquaResult :: [ProcessingUnit] -> [Char]
acquaResult pus =
  let
    specialPu = (head pus)
    Just env = Map.lookup "0" (environments specialPu)
    Just response = Map.lookup "result" env
  in
    "response: " ++ (show response)

acquaResultMap :: [ProcessingUnit] -> [Char]
acquaResultMap pus =
  let
    specialPu = (head pus)
    Just env = Map.lookup "0" (environments specialPu)
    responses :: Int -> String
    responses n = case response n of
                    Just (NumberV resp) -> (show resp) ++ ", " ++ (responses (n+1))
                    Just _ -> error "Result should be a number"
                    Nothing -> ""
    response n = Map.lookup ("result" ++ (show n)) env
  in
    "response: " ++ (responses 0)

