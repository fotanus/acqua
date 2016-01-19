module Simulator.Run where

import Debug.Trace
import Text.Show.Pretty

import AcquaIR.Language
import Simulator.Acqua

import Simulator.Rules
import Simulator.Rules.All

run :: Program -> Int -> String
run program pus_n = step (newAcqua program pus_n)

step :: Acqua -> String
step acqua = _step (applyRules rules acqua) acqua

_step :: Acqua -> Acqua -> String
_step (Acqua _ _ _ _ True _) _ = "Finished!"
_step acqua acqua'=
  if acqua == acqua'
    then error ("Cannot give a step!\n" ++ (ppShow acqua))
    else trace ("----") $ _step (applyRules rules (untaintAll acqua)) acqua
