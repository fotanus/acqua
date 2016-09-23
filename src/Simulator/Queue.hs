module Simulator.Queue where

import qualified Data.List as List

import AcquaIR.Language
import Simulator.ProcessingUnitId
import Simulator.ProcessingUnit as PU
import Simulator.ReturnAddrVar

data Queue = Queue {
  jobs :: [Job],
  locked :: Bool
} deriving(Show, Eq)

data CopySource
  = CallSource Name
  | MapSource Name Int
  deriving (Show,Eq)

data Job = Job {
  label :: Label,
  environment :: EnvId,
  puId :: PId,
  copySource :: CopySource,
  copySourceSize :: Int,
  variable :: ReturnAddrVar
} deriving(Show, Eq)

newQueue :: ProcessingUnit -> Queue
newQueue pu =
  let
    envId = currentEnv pu
    startingJob = Job "main" envId (PU.puId pu) (CallSource envId) 1 (EnvVal "result")
  in
    Queue [startingJob] False

newQueueForMap :: ProcessingUnit -> Int -> Queue
newQueueForMap pu paramsLength =
  let
    envId = currentEnv pu
    startingJobs = List.map (\x-> Job "main" envId (PU.puId pu) (CallSource (show x)) 1 (EnvVal ("result"++(show x)))) [0..((paramsLength)-1)]
  in
    Queue startingJobs False
