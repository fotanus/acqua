module Simulator.Rules.Messages.SendMsg where

import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

sendMsg :: Rule
sendMsg acqua =
    acqua { processingUnits = pus', interconnection = i ++ i' }
  where
    i = interconnection acqua
    (pus', i') = stepSendMsg (processingUnits acqua)

stepSendMsg :: [ProcessingUnit] -> ([ProcessingUnit], Interconnection)
stepSendMsg [] = ([],[])
stepSendMsg (pu:pus) =
  let
    (pus', i') = stepSendMsg pus
  in
    case (outgoingMessageQueue pu) of
      []     -> ((pu:pus'), i')
      (m:ms) -> trace (("[message "++(show (PU.puId pu))++"]") ++ ": sending message " ++ (show m)) $ ((pu':pus'), (m:i'))
        where
          pu' = pu { outgoingMessageQueue = ms }
