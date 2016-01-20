module Simulator.Rules.Base where

import AcquaIR.Language as IR
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

getBB :: Label -> Program -> BasicBlock
getBB l [] = error $ "Basic block not found: " ++ l
getBB l (bb:bbs) =
  if (IR.label bb) == l
    then bb
    else getBB l bbs

