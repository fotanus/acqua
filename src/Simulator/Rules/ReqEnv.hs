module Simulator.Rules.ReqEnv where

import Data.List
import qualified Data.Map as Map
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

reqEnv :: Rule
reqEnv acqua  =
  let
    Acqua bb q pus i f s = acqua
  in case i of
      ((ConstMsgReqEnv (MsgReqEnv pIdS mjsId pIdT mteId)):ms) -> trace ((show (PU.puId pu)) ++ ": receive ReqEnv")  $ Acqua bb q pus' ms f s
        where
          Just pu = Data.List.find (\p -> (PU.puId p) == pIdT) pus
          PU _ c t ce env ra cc se omq enbl _ = pu

          omq' = omq ++ newMessages
          newMessages = updMsgs ++ [endMsg]
          endMsg = (ConstMsgEndCopy (MsgEndCopy pIdS))
          updMsgs = map (\nv -> (ConstMsgUpdate (MsgUpdate pIdS mjsId (fst nv) (snd nv) ))) (Map.toList copyEnv)
          Just copyEnv = Map.lookup mteId env

          pu' = PU pIdT c t ce env ra cc se omq' enbl True
          pus' = updatePU pus pu'

      _ -> acqua
