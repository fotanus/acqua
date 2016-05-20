module Simulator.Rules.ReqEnv where

import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Environment


import Simulator.Rules.Base

reqEnv :: Rule
reqEnv acqua  =
  let
    Acqua bb q pus i f s = acqua
  in case i of
      ((ConstMsgReqEnv (MsgReqEnv pIdS mjsId pIdT mteId closureName)):ms) -> trace ((show (PU.puId pu)) ++ ": receive ReqEnv")  $ Acqua bb q pus' ms f s
        where
          Just pu = Data.List.find (\p -> (PU.puId p) == pIdT) pus
          PU _ c t ce env ra cc se omq enbl _ = pu

          omq' = omq ++ newMessages
          newMessages = updMsgs ++ [endMsg]
          endMsg = (ConstMsgEndCopy (MsgEndCopy pIdS))
          updMsgs = toList $ Seq.mapWithIndex idxValToMsg (params closure)
          Just tenv = Map.lookup mteId env
          Just (ClosureV closure) = Map.lookup closureName tenv
          idxValToMsg idx val =
              ConstMsgUpdate (MsgUpdate pIdS mjsId idx val)

          pu' = PU pIdT c t ce env ra cc se omq' enbl True
          pus' = updatePU pus pu'

      _ -> acqua
