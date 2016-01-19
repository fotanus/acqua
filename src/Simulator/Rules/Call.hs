module Simulator.Rules.Call where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Queue
import Simulator.Environment

import Simulator.Rules

call :: Rule
call (Acqua bb q pus i f) =
    Acqua bb q' pus' i f
  where
    (q', pus') = stepCall q pus

stepCall :: Queue -> [ProcessingUnit] -> (Queue, [ProcessingUnit])
stepCall q [] = (q,[])
stepCall q (pu:pus) =
  case PU.commands pu of
    ((Call x1 x2 envId):cs) -> if PU.tainted pu == False
                                 then trace ((show (PU.puId pu)) ++  ": call") (q'', pu':pus')
                                 else let
                                        (q', pus') = stepCall q pus
                                      in
                                        (q', pu:pus')

      where
        (q'', pus') = stepCall q' pus
        -- queue
        Just cenv = Map.lookup ce rEnv
        Just (LabelValue l) = Map.lookup x2 cenv
        l' = case Map.lookup l cenv of
                     Just (LabelValue l') -> l'
                     Nothing -> l
        j = Job l' envId pId ce x1
        q' = q ++ [j]

        -- pu
        PU pId _ t ce rEnv cEnv ra cc se _ = pu
        Just nCalls = Map.lookup ce cc
        cc' = Map.insert ce (nCalls+1) cc
        pu' = PU pId cs t ce rEnv cEnv ra cc' se True

    _ -> (q', pu:pus')
      where (q', pus') = stepCall q pus

