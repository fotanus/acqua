module Simulator.Queue where

import qualified Data.List as List

import Simulator.Job
import Simulator.ProcessingUnit as PU
import Simulator.ReturnAddrVar
import Simulator.Value

data Queue = Queue {
  jobs :: [Job],
  locked :: Bool
} deriving(Show, Eq)

addJobs :: Queue -> [Job] -> Queue
addJobs q js = q { jobs = (jobs q) ++ js }

newQueueForMap :: ProcessingUnit -> Int -> Queue
newQueueForMap pu paramsLength =
  let
    envId = currentEnv pu
    startingJobs = List.map (\x-> Job (PU.puId pu) (CallSource (Pointer (PU.puId pu) x)) 1 envId (EnvVal ("result"++(show x))) True) [0..((paramsLength)-1)]
  in
    Queue startingJobs False
