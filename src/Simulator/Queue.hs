module Simulator.Queue where

import qualified Data.List as List

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
  callRecord :: Name,
  variable :: Name
} deriving(Show, Eq)

newQueue :: ProcessingUnit -> Queue
newQueue pu =
  let
    envId = currentEnv pu
    startingJob = Job "main" envId (PU.puId pu) envId "result"
  in
    Queue [startingJob] False

newQueueForMap :: ProcessingUnit -> [Int] -> Queue
newQueueForMap pu pars =
  let
    envId = currentEnv pu
    startingJobs = List.map (\x-> Job "main" envId (PU.puId pu) (show x) ("result"++(show x))) [0..((length pars)-1)]
  in
    Queue startingJobs False
