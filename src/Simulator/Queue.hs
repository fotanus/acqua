module Simulator.Queue where

import AcquaIR.Language
import Simulator.ProcessingUnit

type Queue = [Job]

data Job = Job {
  l :: Label,
  envId :: EnvId,
  pId :: PId,
  env :: EnvId,
  var :: Name
}

newQueue :: Queue
newQueue = []
