module Simulator.ProcessingUnit where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as ListSP

import AcquaIR.Language
import Simulator.Value
import Simulator.List
import Simulator.Environment
import Simulator.CallRecordSeg
import Simulator.Interconnection
import Simulator.CallRecord
import Simulator.ProcessingUnitId
import Simulator.ReturnAddrVar

data ReturnAddr = ReturnAddr {
  addr_pId :: PId,
  addr_envId :: EnvId,
  variable :: ReturnAddrVar
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
  callRecordSeg :: CallRecordSeg,
  returnAddrs :: Map.Map EnvId ReturnAddr,
  originCallRec :: Map.Map EnvId Pointer,
  callCount :: Map.Map EnvId Int,
  sleepingExecution :: Map.Map EnvId ExecutionContext,
  outgoingMessageQueue :: [Message],
  enabled :: Bool,
  locked :: Bool,
  stallCycles :: Int,
  lockedMsg :: Bool
} deriving (Show,Eq)

emptySpecialPU :: ProcessingUnit
emptySpecialPU =
  let
    environmentZero = Map.fromList $ [("0", PointerV (Pointer 0 0))] ++ [("resultType", (NumberV 0))]
  in
    PU {
        Simulator.ProcessingUnit.puId=0,
        Simulator.ProcessingUnit.commands = [],
        Simulator.ProcessingUnit.terminator = Empty,
        Simulator.ProcessingUnit.currentEnv = "0",
        Simulator.ProcessingUnit.environments = Map.fromList [("0", environmentZero)],
        Simulator.ProcessingUnit.callRecordSeg = Map.fromList [(0, CallRecordV emptyCallRecord)],
        Simulator.ProcessingUnit.returnAddrs = Map.fromList [],
        Simulator.ProcessingUnit.originCallRec = Map.fromList [],
        Simulator.ProcessingUnit.callCount = Map.fromList [("0",1)],
        Simulator.ProcessingUnit.sleepingExecution = Map.fromList [("0", ExecutionContext [] Empty)],
        Simulator.ProcessingUnit.outgoingMessageQueue = [],
        Simulator.ProcessingUnit.enabled = False,
        Simulator.ProcessingUnit.locked = False,
        Simulator.ProcessingUnit.stallCycles = 0,
        Simulator.ProcessingUnit.lockedMsg = False
    }

specialPU :: [String] -> ProcessingUnit
specialPU pars =
  let
    env = Map.fromList $ (List.map (\i -> ((show i), PointerV (Pointer 0 i))) [0..(length pars)]) ++ [("resultType", NumberV 1)]
    envs = Map.fromList [("0", env)]
    startingListAddr = length pars
    crseg = Map.fromList (List.foldr paramsToCRegList [] (zip [0..] pars))

    parseListToInts l = map (\a -> NumberV (read a)) $ ListSP.splitOn "," $ tail.reverse.tail.reverse $ l

    paramsToCRegList (idx,param) acm =
      case head param of
              '[' -> acm ++ [
                        (idx,                 CallRecordV (callRecordWithListParam (Pointer 0 (startingListAddr + idx)))),
                        (startingListAddr + idx, ListV (List (length (parseListToInts param)) (parseListToInts param)))
                     ]
              _   -> acm ++ [(idx, CallRecordV (callRecordWithIntParam (read param)))]
  in
    PU {
        Simulator.ProcessingUnit.puId=0,
        Simulator.ProcessingUnit.commands = [],
        Simulator.ProcessingUnit.terminator = Empty,
        Simulator.ProcessingUnit.currentEnv = "0",
        Simulator.ProcessingUnit.environments = envs,
        Simulator.ProcessingUnit.callRecordSeg = crseg,
        Simulator.ProcessingUnit.returnAddrs = Map.fromList [],
        Simulator.ProcessingUnit.originCallRec = Map.fromList [],
        Simulator.ProcessingUnit.callCount = Map.fromList [("0",(length pars))],
        Simulator.ProcessingUnit.sleepingExecution = Map.fromList [("0", ExecutionContext [] Empty)],
        Simulator.ProcessingUnit.outgoingMessageQueue = [],
        Simulator.ProcessingUnit.enabled = False,
        Simulator.ProcessingUnit.locked = False,
        Simulator.ProcessingUnit.stallCycles = 0,
        Simulator.ProcessingUnit.lockedMsg = False
    }

newPU :: Int -> ProcessingUnit
newPU n =
    PU {
        Simulator.ProcessingUnit.puId=n,
        Simulator.ProcessingUnit.commands = [],
        Simulator.ProcessingUnit.terminator = Empty,
        Simulator.ProcessingUnit.currentEnv = "",
        Simulator.ProcessingUnit.environments = Map.fromList [],
        Simulator.ProcessingUnit.callRecordSeg = Map.fromList [],
        Simulator.ProcessingUnit.returnAddrs = Map.fromList [],
        Simulator.ProcessingUnit.originCallRec = Map.fromList [],
        Simulator.ProcessingUnit.callCount = Map.fromList [],
        Simulator.ProcessingUnit.sleepingExecution = Map.fromList [],
        Simulator.ProcessingUnit.outgoingMessageQueue = [],
        Simulator.ProcessingUnit.enabled = False,
        Simulator.ProcessingUnit.locked = False,
        Simulator.ProcessingUnit.stallCycles = 0,
        Simulator.ProcessingUnit.lockedMsg = False
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
unlock pu = pu { locked = False, lockedMsg = False }

decrementStallCycles :: ProcessingUnit -> ProcessingUnit
decrementStallCycles pu =
  pu { stallCycles = decrementedStallCycles }
  where
    decrementedStallCycles = if stallCycles pu > 0 then (stallCycles pu) - 1 else 0


canExecuteCmds :: ProcessingUnit -> Bool
canExecuteCmds pu = (enabled pu) && (not (locked pu)) && (stallCycles pu) == 0

currentPuEnv :: ProcessingUnit -> Environment
currentPuEnv pu =
  let
    Just cenv = Map.lookup (currentEnv pu) (environments pu)
  in
    cenv
