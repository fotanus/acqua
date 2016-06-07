module Simulator.Rules.Return where

import qualified Data.Map as Map
import Logger
import Text.Show.Pretty

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
    ([], Return x,True) -> trace ((show (PU.puId pu)) ++ ": returning " ++ (show m)) (i'', pu':pus')
      where
        (i'', pus') = stepReturn i' pus
        -- interconnection
        Just cenv = Map.lookup ce env
        Just returnValue = Map.lookup x cenv
        Just (ReturnAddr pId' envId' x') = Map.lookup ce ra
        m = MsgResponse pId' envId' x' returnValue
        i' = (ConstMsgResponse m) : i

        -- pu
        ce = currentEnv pu
        cc = callCount pu
        ra = returnAddrs pu
        env = environments pu
        cc' = Map.insert ce 100 cc
        pu' = pu { PU.commands = [], PU.terminator = Empty, callCount = cc', locked = True }

    _ -> (i', pu:pus')
      where (i', pus') = stepReturn i pus

