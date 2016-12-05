module Simulator.Rules.Return where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.Value
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

returnTerminator :: Rule
returnTerminator acqua =
    acqua { processingUnits = pus', interconnection = i' }
  where
    (i', pus') = stepReturn acqua (interconnection acqua) (processingUnits acqua)

stepReturn :: Acqua -> Interconnection -> [ProcessingUnit] -> (Interconnection, [ProcessingUnit])
stepReturn _ i [] = (i,[])
stepReturn acqua i (pu:pus) =
  case (PU.commands pu,PU.terminator pu,PU.canExecuteCmds pu) of
    ([], Return x,True) -> trace ((show (PU.puId pu)) ++ ": returning " ++ (show m)) (i'', pu':pus')
      where
        (i'', pus') = stepReturn acqua i' pus
        -- interconnection
        Just cenv = Map.lookup ce env
        Just returnValue = Map.lookup x cenv
        Just (ReturnAddr pId' envId' x') = Map.lookup ce ra
        isMapF = case Map.lookup "isMap" cenv of
                 Just (NumberV 1) -> True
                 _ -> False
        m = MsgResponse pId' envId' x' returnValue isMapF
        i' = (ConstMsgResponse m (msgStepsToPropagate acqua)) : i

        -- pu
        ce = currentEnv pu
        cc = callCount pu
        ra = returnAddrs pu
        env = environments pu
        cc' = Map.insert ce 100 cc
        pu' = pu { PU.commands = [], PU.free = True, PU.terminator = Empty, callCount = cc', locked = True }

    _ -> (i', pu:pus')
      where (i', pus') = stepReturn acqua i pus

