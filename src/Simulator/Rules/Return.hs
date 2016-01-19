module Simulator.Rules.Return where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules

returnTerminator :: Rule
returnTerminator (Acqua bb q pus i f) =
    Acqua bb q pus' i' f
  where
    (i', pus') = stepReturn i pus

stepReturn :: Interconnection -> [ProcessingUnit] -> (Interconnection, [ProcessingUnit])
stepReturn i [] = (i,[])
stepReturn i (pu:pus) =
  case (PU.commands pu,PU.terminator pu) of
    ([], Return x) -> trace "return" (i'', pu':pus')
      where
        (i'', pus') = stepReturn i' pus
        -- interconnection
        Just cenv = Map.lookup ce rEnv
        Just returnValue = Map.lookup x cenv
        Just (ReturnAddr pId' envId' x') = Map.lookup ce ra
        m = Message pId' envId' x' returnValue
        i' = m : i

        -- pu
        PU pId _ _ ce rEnv cEnv ra cc se = pu
        cc' = Map.insert ce 0 cc
        pu' = PU pId [] Empty ce rEnv cEnv ra cc' se

    _ -> (i', pu:pus')
      where (i', pus') = stepReturn i pus
