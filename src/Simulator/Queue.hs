module Simulator.Queue where

import qualified Data.List as List

import AcquaIR.Language
import Simulator.ProcessingUnitId
import Simulator.ProcessingUnit as PU
import Simulator.ReturnAddrVar
import Simulator.Value

data Queue = Queue {
  jobs :: [Job],
  locked :: Bool
} deriving(Show, Eq)

data CopySource
  = CallSource Pointer 
  | MapSource Pointer Int
  deriving (Show,Eq)

data Job = Job {
  puId :: PId,
  copySource :: CopySource,
  copySourceSize :: Int,
  environment :: EnvId,
  variable :: ReturnAddrVar
} deriving(Show, Eq)

newQueueForMap :: ProcessingUnit -> Int -> Queue
newQueueForMap pu paramsLength =
  let
    envId = currentEnv pu
    startingJobs = List.map (\x-> Job (PU.puId pu) (CallSource (Pointer (PU.puId pu) x)) 1 envId (EnvVal ("result"++(show x)))) [0..((paramsLength)-1)]
  in
    Queue startingJobs False
