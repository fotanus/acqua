module Simulator.Rules where

import Simulator.Rules.Base
import Simulator.Rules.AssignJob
import Simulator.Rules.Assign
import Simulator.Rules.Return
import Simulator.Rules.If
import Simulator.Rules.Goto
import Simulator.Rules.ReceiveResponse
import Simulator.Rules.ReceiveUpdate
import Simulator.Rules.ReceiveUpdateClos
import Simulator.Rules.ReceiveUpdateMetaClos
import Simulator.Rules.ReceiveUpdateList
import Simulator.Rules.ReceiveUpdateMetaList
import Simulator.Rules.Resume
import Simulator.Rules.Call
import Simulator.Rules.Wait
import Simulator.Rules.Op
import Simulator.Rules.ReqEnv
import Simulator.Rules.ReqClos
import Simulator.Rules.SendEnvMsg
import Simulator.Rules.EndCopy
import Simulator.Rules.StepMessage

import Simulator.Rules.NewList
import Simulator.Rules.ListSet
import Simulator.Rules.ListSetN
import Simulator.Rules.Head
import Simulator.Rules.Tail
import Simulator.Rules.Last
import Simulator.Rules.Length
import Simulator.Rules.Concat
import Simulator.Rules.Map
import Simulator.Rules.Filter

import Simulator.Rules.NewCallRecord
import Simulator.Rules.SetCallRecordFn
import Simulator.Rules.SetCallRecordMissing
import Simulator.Rules.SetCallRecordMissingI
import Simulator.Rules.SetCallRecordCount
import Simulator.Rules.SetCallRecordCountI
import Simulator.Rules.GetCallRecordParam
import Simulator.Rules.SetCallRecordParam
import Simulator.Rules.SetCallRecordParamI
import Simulator.Rules.SetCallRecordParamIL
import Simulator.Rules.GetCallRecordMissing
import Simulator.Rules.GetCallRecordCount


rules :: [Rule]
rules = [
  assignV, assignL, assignI, returnTerminator, resume, wait, op, ifRule, goto,
  newList, listSet, listSetN, headRule, tailRule, lastRule, lengthRule, concatRule, mapRule, filterRule,
  newCallRecord, setCallRecordFn, setCallRecordMissingI, setCallRecordCountI, setCallRecordParam, getCallRecordParam,
  setCallRecordParamIL, setCallRecordParamI, getCallRecordMissing, getCallRecordCount, setCallRecordCount, setCallRecordMissing,
  receiveResponse, receiveUpdate, receiveUpdateClos, receiveUpdateList, receiveUpdateMetaClos, receiveUpdateMetaList, endCopy,
  sendEnvMsg, reqEnv, reqClos, assignJob, call, stepMessage
  ]

