module Simulator.Run where


import AcquaIR.Language
import Simulator.Acqua

import Simulator.Rules

run :: Program -> Int -> String
run program pus_n = step (newAcqua program pus_n)

step :: Acqua -> String
step (Acqua _ _ _ _ True) = "Finished!"
step acqua = step (applyRules rules acqua)
