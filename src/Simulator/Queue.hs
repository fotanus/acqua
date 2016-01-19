module Simulator.Queue where

import AcquaIR.Language
import Simulator.ProcessingUnit

type Queue = [Job]

data Job = Job {
  label :: Label,
  environmentId :: EnvId,
  puId :: PId,
  environment :: EnvId,
  variable :: Name
} deriving(Show, Eq)

newQueue :: ProcessingUnit -> Queue
newQueue pu =
  let PU pId _ _ envId _ _ _ _ _ = pu
  in [(Job "main" envId pId envId "result")]
