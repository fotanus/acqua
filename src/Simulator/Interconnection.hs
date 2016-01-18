module Simulator.Interconnection where

import AcquaIR.Language
import Simulator.ProcessingUnit
import Simulator.Environment


type Interconnection = [Message]

data Message = Message {
  puId :: PId,
  environment :: EnvId,
  variable :: Name,
  value :: Value
  }

newInterconnection :: Interconnection
newInterconnection = []
