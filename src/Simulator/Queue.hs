module Simulator.Queue where

import AcquaIR.Language
import Simulator.ProcessingUnit

data Queue = Queue {
  jobs :: [Job],
  locked :: Bool
} deriving(Show, Eq)

data Job = Job {
  label :: Label,
  environmentId :: EnvId,
  puId :: PId,
  environment :: EnvId,
  variable :: Name
} deriving(Show, Eq)

newQueue :: ProcessingUnit -> Queue
newQueue pu =
  let PU pId _ _ envId _ _ _ _ _ _ _ = pu
  in Queue [(Job "main" envId pId envId "result")] False
