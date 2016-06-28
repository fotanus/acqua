module Simulator.Acqua where

import qualified Data.Map as Map

import AcquaIR.Language as IR
import Simulator.AcquaState
import Simulator.ProcessingUnit
import Simulator.Interconnection
import Simulator.Environment
import Simulator.Queue
import Simulator.Value

type FinishFlag = Bool

data Acqua = Acqua {
  program :: Program,
  queue :: Queue,
  processingUnits :: [ProcessingUnit],
  interconnection :: Interconnection,
  finishFlag :: FinishFlag,
  acquaState :: AcquaState
} deriving (Show,Eq)

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
    addGetVar bb = bb { IR.commands = ((GetCallRecordParam "callRecord" 0 var):(IR.commands bb)) }

unlockAll :: Acqua -> Acqua
unlockAll (Acqua bb q pus i ff s)
  = Acqua bb q pus' i ff s
  where
    pus' = map (\p -> unlock p) pus

showAcquaResult :: Acqua -> String
showAcquaResult acqua =
  let
    specialPu = (head (processingUnits acqua))
    Just responseEnv = Map.lookup "0" (environments specialPu)
    stats = showStats acqua
    result = case Map.lookup "resultType" responseEnv of
        Just (NumberV 0) -> acquaResultRun responseEnv
        Just (NumberV 1) -> acquaResultMap responseEnv
        _ -> error "Can't show this result type"
  in
    result ++ "\n\n" ++ stats

showStats :: Acqua -> String
showStats acqua =
  let
    statsAndFormat = [
        ("occupiedPUPerCycle", (\(MapIntInt v) -> maximum (Map.elems v))),
        ("maxQueueSize",       (\(IntVal v)    -> v)),
        ("maxCallRec",         (\(IntVal v)    -> v)),
        ("maxLocalCallRec",    (\(IntVal v)    -> v)),
        ("maxMsg",             (\(IntVal v)    -> v))
        ]
    statLineFormat s val = s ++ ": " ++ val ++ "\n"
    statLines = map (\(s,f) -> statLineFormat s (show (f ((acquaState acqua) Map.! s)))) statsAndFormat
  in
    foldr (++) "----stats----\n" statLines

acquaResultRun :: Environment -> [Char]
acquaResultRun env =
  let
    Just response = Map.lookup "result" env
  in
    "response: " ++ (show response)

acquaResultMap :: Environment -> [Char]
acquaResultMap env =
  let
    responses :: Int -> String
    responses n = case response n of
                    Just (NumberV resp) -> (show resp) ++ ", " ++ (responses (n+1))
                    Just _ -> error "Result should be a number"
                    Nothing -> ""
    response n = Map.lookup ("result" ++ (show n)) env
  in
    "response: " ++ (responses 0)
