module Simulator.Rules.Base where

import Simulator.Acqua
import Simulator.ProcessingUnit as PU


type Rule = Acqua -> Acqua

-- helpers
updatePU :: [ProcessingUnit] -> ProcessingUnit -> [ProcessingUnit]
updatePU [] p' = error $ "Trying to update an unexisting PU: " ++ (show p')
updatePU (x:xs) p' =
  if (PU.puId x) == (PU.puId p')
    then p' : xs
    else x : updatePU xs p'
