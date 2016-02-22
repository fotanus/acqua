module Simulator.Rules.Call where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Queue
import Simulator.Environment

import Simulator.Rules.Base

call :: Rule
call (Acqua bb q pus i f s) =
    Acqua bb q' pus' i f s
  where
    (q', pus') = stepCall q pus s

stepCall :: Queue -> [ProcessingUnit] -> Map.Map String StateValue -> (Queue, [ProcessingUnit])
stepCall q [] _ = (q,[])
stepCall q (pu:pus) s =
  case (PU.commands pu,PU.tainted pu) of
    ((Call x1 x2 envId):cs,False) -> trace ((show (PU.puId pu)) ++  ": call" ) (q'', pu':pus')
      where
        (q'', pus') = stepCall q' pus s
        -- queue
        Just cenv = Map.lookup ce rEnv
        Just (LabelValue l) = Map.lookup x2 cenv
        l' = case Map.lookup l cenv of
                     Just (LabelValue l'') -> l''
                     Nothing -> l
                     _ -> error "Calling a value that is not a label"

        (Just (NewEnvIds envMap)) = Map.lookup "newEnvIds" s
        (Just copyEnvId) = Map.lookup (pId,envId) envMap
        j = Job l' copyEnvId pId ce x1
        q' = q ++ [j]

        -- pu
        PU pId _ t ce rEnv cEnv ra cc se _ = pu
        Just nCalls = Map.lookup ce cc
        cc' = Map.insert ce (nCalls+1) cc
        pu' = PU pId cs t ce rEnv cEnv ra cc' se True

    _ -> (q', pu:pus')
      where (q', pus') = stepCall q pus s
