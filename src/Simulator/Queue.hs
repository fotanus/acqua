module Simulator.Queue where

import AcquaIR.Language
import Simulator.ProcessingUnitId
import Simulator.ProcessingUnit as PU

data Queue = Queue {
  jobs :: [Job],
  locked :: Bool
} deriving(Show, Eq)

data Job = Job {
  label :: Label,
  environment :: EnvId,
  puId :: PId,
  closure :: Name,
  variable :: Name
} deriving(Show, Eq)

newQueue :: ProcessingUnit -> Queue
newQueue pu =
  let
    envId = currentEnv pu
    startingJob = Job "main" envId (PU.puId pu) envId "result"
  in
    Queue [startingJob] False
