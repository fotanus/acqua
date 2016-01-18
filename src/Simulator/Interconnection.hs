module Simulator.Interconnection where

import AcquaIR.Language
import Simulator.ProcessingUnit
import Simulator.Environment


type Interconnection = [Message]

data Message = Message {
  pId :: PId,
  env :: EnvId,
  var :: Name,
  v :: Value
}

newInterconnection :: Interconnection
newInterconnection = []
