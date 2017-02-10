module Simulator.Rules.Delete where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value

import Simulator.Rules.Base

deleteRule :: Rule
deleteRule acqua =
    acqua { processingUnits = stepDelete (processingUnits acqua) }

stepDelete :: [ProcessingUnit] -> [ProcessingUnit]
stepDelete [] = []
stepDelete (pu:pus) =
  case (PU.commands pu, PU.canExecuteCmds pu) of
    ((Delete x1):cs,True) -> trace ((show (PU.puId pu)) ++  ": delete " ++ x1 ) (pu':pus')
      where
        ce = PU.currentEnv pu
        envs = PU.environments pu
        Just cenv = Map.lookup ce envs
        crseg = PU.callRecordSeg pu

        Just (PointerV pointer) = Map.lookup x1 cenv
        crseg' = Map.delete (addr pointer) crseg

        pu' =  pu { PU.commands = cs, callRecordSeg = crseg', PU.locked = True }
        pus' = stepDelete pus

    _ -> pu:(stepDelete pus)
