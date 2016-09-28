module Simulator.Acqua where

import Data.List
import qualified Data.Map as Map

import AcquaIR.Language as IR
import Simulator.AcquaState
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg as CRS
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
  acquaState :: AcquaState,
  msgStepsToPropagate :: Int
} deriving (Show)

instance Eq Acqua where
  Acqua a b c d e _ f == Acqua a' b' c' d' e' _ f' =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f'


newAcqua :: Program -> Int -> Int -> String -> [String] -> Acqua
newAcqua p n stepsToPropagate var params = Acqua p' q pus newInterconnection False statesDefault stepsToPropagate
  where
    specialProcessingUnit = specialPU params
    pus = specialProcessingUnit:(newProcessingUnits n)
    q = newQueueForMap specialProcessingUnit (length params)
    p' = ((addGetVar (head p)):(tail p))
    addGetVar bb = bb { IR.commands = ((GetCallRecordParam "callRecord" 0 var):(IR.commands bb)) }

unlockAndUnstallAll :: Acqua -> Acqua
unlockAndUnstallAll acqua = acqua { processingUnits = (map (unlock.decrementStallCycles) (processingUnits acqua)) }

showAcquaResult :: Acqua -> String
showAcquaResult acqua =
  let
    specialPu = (head (processingUnits acqua))
    Just responseEnv = Map.lookup "0" (environments specialPu)
    stats = showStats acqua
    result = case Map.lookup "resultType" responseEnv of
        Just (NumberV 0) -> acquaResultRun responseEnv
        Just (NumberV 1) -> acquaResultMap responseEnv acqua
        _ -> error "Can't show this result type"
  in
    result ++ "\n\n" ++ stats

showStats :: Acqua -> String
showStats acqua =
  let
    statsAndFormat = [
        ("occupiedPUPerCycle",  "occupiedPUPerCycle",  (\(MapIntInt v) -> (show (maximum (Map.elems v))))),
        ("meanPUPerCycle",      "occupiedPUPerCycle",  (\(MapIntInt v) -> (show ((fromIntegral (sum (Map.elems v)) :: Float) / (fromIntegral (length (Map.elems v)) :: Float))))),
        ("maxQueueSize",        "maxQueueSize",        (\(IntVal v)    -> (show v))),
        ("maxCallRec",          "maxCallRec",          (\(IntVal v)    -> (show v))),
        ("maxMsg",              "maxMsg",              (\(IntVal v)    -> (show v))),
        ("steps",               "steps",               (\(IntVal v)    -> (show v)))
        ]
    statLineFormat l val = l ++ ": " ++ val ++ "\n"
    statLines = map (\(l,s,f) -> statLineFormat l (f ((acquaState acqua) Map.! s))) statsAndFormat
  in
    foldl (++) "----stats----\n" statLines

acquaResultRun :: Environment -> [Char]
acquaResultRun env =
  let
    Just response = Map.lookup "result" env
  in
    "response: " ++ (show response)

acquaResultMap :: Environment -> Acqua -> [Char]
acquaResultMap env acqua =
  let
    responses :: Int -> String
    responses n = case response n of
                    Just (NumberV resp) ->
                        (show resp) ++ ", " ++ (responses (n+1))
                    Just (PointerV (Pointer pid pos)) ->
                        let
                          Just puWithResp = find (\pu -> (PU.puId pu)==pid) (processingUnits acqua)
                          resp = CRS.lookup pos (callRecordSeg puWithResp)
                        in
                          (show resp) ++ ", " ++ (responses (n+1))
                    Just resp -> error $ "Unexpected value for result: " ++ (show resp)
                    Nothing -> ""
    response n = Map.lookup ("result" ++ (show n)) env
  in
    "response: " ++ (responses 0)
