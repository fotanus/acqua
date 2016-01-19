module Simulator.Rules where

import Simulator.Acqua
import Simulator.ProcessingUnit as PU

type Rule = Acqua -> Acqua

applyRules :: [Rule] -> Acqua -> Acqua
applyRules [] a = a
applyRules (f:fs) a = applyRules fs (f a)

updatePU :: [ProcessingUnit] -> ProcessingUnit -> [ProcessingUnit]
updatePU [] p' = error $ "Trying to update an unexisting PU: " ++ (show p')
updatePU (x:xs) p' =
  if (PU.puId x) == (PU.puId p')
    then p' : xs
    else x : updatePU xs p'
