module Simulator.Rules.StepJob where

import Logger

import Simulator.Acqua
import Simulator.Job
import Simulator.Queue
import Simulator.ProcessingUnit as PU

import Simulator.Rules.Base

stepJob :: Rule
stepJob acqua =
    acqua { processingUnits = pus', queue = addJobs (queue acqua) js }
  where
    (pus', js) = stepStepJob (processingUnits acqua)

stepStepJob :: [ProcessingUnit] -> ([ProcessingUnit], [Job])
stepStepJob [] = ([],[])
stepStepJob (pu:pus) =
  let
    (pus', js') = stepStepJob pus
  in
    case (outgoingJobQueue pu) of
      []     -> ((pu:pus'), js')
      (j:js) -> trace ((show pId) ++ ": sending job from outgoing job queue") $ ((pu':pus'), (j:js'))
        where
          pId = PU.puId pu
          pu' = pu { outgoingJobQueue = js }
