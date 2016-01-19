module Simulator.Rules where

import Simulator.Acqua

type Rule = Acqua -> Acqua

applyRules :: [Rule] -> Acqua -> Acqua
applyRules [] a = a
applyRules (f:fs) a = applyRules fs (f a)
