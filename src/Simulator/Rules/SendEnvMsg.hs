module Simulator.Rules.SendEnvMsg where

import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base


sendEnvMsg :: Rule
sendEnvMsg (Acqua bb q pus i f s) =
    Acqua bb q pus' (i++i') f s
  where
    (pus', i') = stepSendEnvMsg pus

stepSendEnvMsg :: [ProcessingUnit] -> ([ProcessingUnit], Interconnection)
stepSendEnvMsg [] = ([],[])
stepSendEnvMsg (pu:pus) =
  let
    (pus', i') = stepSendEnvMsg pus
  in
    case (outgoingMessageQueue pu) of
      []     -> ((pu:pus'), i')
      (m:ms) -> trace ((show pId) ++ ": sending message for env copy" ++ (show m)) $ ((pu':pus'), (m:i'))
        where
          pId = PU.puId pu
          pu' = pu { outgoingMessageQueue = ms }
