module Simulator.Rules.Return where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

returnTerminator :: Rule
returnTerminator (Acqua bb q pus i f s) =
    Acqua bb q pus' i' f s
  where
    (i', pus') = stepReturn i pus

stepReturn :: Interconnection -> [ProcessingUnit] -> (Interconnection, [ProcessingUnit])
stepReturn i [] = (i,[])
stepReturn i (pu:pus) =
  case (PU.commands pu,PU.terminator pu,PU.canExecuteCmds pu) of
    ([], Return x,True) -> trace ((show (PU.puId pu)) ++ ": returning ") (i'', pu':pus')
      where
        (i'', pus') = stepReturn i' pus
        -- interconnection
        Just cenv = Map.lookup ce rEnv
        Just returnValue = Map.lookup x cenv
        Just (ReturnAddr pId' envId' x') = Map.lookup ce ra
        m = Message pId' envId' x' returnValue
        i' = m : i

        -- pu
        PU pId _ _ ce rEnv cEnv ra cc se enbl _ = pu
        cc' = Map.insert ce 100 cc
        pu' = PU pId [] Empty ce rEnv cEnv ra cc' se enbl True

    _ -> (i', pu:pus')
      where (i', pus') = stepReturn i pus

