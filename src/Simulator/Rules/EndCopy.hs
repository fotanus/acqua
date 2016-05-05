module Simulator.Rules.EndCopy where

import Data.List
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

endCopy :: Rule
endCopy acqua  =
  let
    Acqua bb q pus i f s = acqua
  in case i of
      ((ConstMsgEndCopy (MsgEndCopy pId)):ms) -> trace ((show (PU.puId pu)) ++ ": receive endCopy")  $ Acqua bb q pus' ms f s
        where
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          PU _ c t ce rEnv cEnv ra cc se omq _ lck = pu
          pu' = PU pId c t ce rEnv cEnv ra cc se omq True lck
          pus' = updatePU pus pu'
      _ -> acqua
