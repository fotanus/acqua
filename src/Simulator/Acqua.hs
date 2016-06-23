module Simulator.Acqua where

import qualified Data.Map as Map

import AcquaIR.Language as IR
import Simulator.ProcessingUnit
import Simulator.Interconnection
import Simulator.Queue

type FinishFlag = Bool
data StateValue = IntVal Int
                | NewEnvIds (Map.Map (Int,String) String)
                deriving (Eq,Show)

type AcquaState = Map.Map String StateValue

data Acqua = Acqua {
  program :: Program,
  queue :: Queue,
  processingUnits :: [ProcessingUnit],
  interconnection :: Interconnection,
  finishFlag :: FinishFlag,
  acquaState :: AcquaState
} deriving (Show,Eq)

statesDefault :: AcquaState
statesDefault = Map.fromList [
    ("newEnvIds", (NewEnvIds (Map.fromList []))),
    ("envId",IntVal 0)
  ]

newAcqua :: Program -> Int -> Acqua
newAcqua p n = Acqua p q pus newInterconnection False statesDefault
  where
    specialProcessingUnit = emptySpecialPU
    pus = specialProcessingUnit:(newProcessingUnits n)
    q = newQueue specialProcessingUnit

newAcquaMap :: Program -> Int -> String -> [String] -> Acqua
newAcquaMap p n var params = Acqua p' q pus newInterconnection False statesDefault
  where
    specialProcessingUnit = specialPU (map read params)
    pus = specialProcessingUnit:(newProcessingUnits n)
    q = newQueueForMap specialProcessingUnit (map read params)
    p' = ((addGetVar (head p)):(tail p))
    addGetVar bb = bb { IR.commands = ((GetClosureParam "closure" 0 var):(IR.commands bb)) }

getNextEnvId :: Map.Map String StateValue -> (EnvId, Map.Map String StateValue)
getNextEnvId s = (nextEnvId,s')
  where
    Just (IntVal count) = Map.lookup "envId" s
    s' = Map.insert "envId" (IntVal (count+1)) s
    nextEnvId = "env_" ++ (show count)

unlockAll :: Acqua -> Acqua
unlockAll (Acqua bb q pus i ff s)
  = Acqua bb q pus' i ff s
  where
    pus' = map (\p -> unlock p) pus
