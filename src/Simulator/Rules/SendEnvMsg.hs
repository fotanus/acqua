module Simulator.Rules.SendEnvMsg where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU

import Simulator.Rules.Base


sendEnvMsg :: Rule
sendEnvMsg (Acqua bb q pus i f s) =
    Acqua bb q pus' (i++i') f s
  where
    (pus', i') = stepSendEnvMsg pus

    stepSendEnvMsg [] = ([],[])
    stepSendEnvMsg (pu:pus) =
      let 
        (pus', i') = stepSendEnvMsg pus
      in
        case (outgoingMessageQueue pu) of
          []     -> ((pu:pus'), i')
          (m:ms) -> trace ((show pId) ++ ": sending message for env copy") $ ((pu':pus'), (m:i'))
            where
              PU pId c t ce rEnv cEnv ra cc se omq enbl lck = pu
              pu' = PU pId c t ce rEnv cEnv ra cc se ms enbl lck
