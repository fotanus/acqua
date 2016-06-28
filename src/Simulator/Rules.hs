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
import Simulator.Rules.Resume
import Simulator.Rules.Call
import Simulator.Rules.Wait
import Simulator.Rules.Op
import Simulator.Rules.ReqEnv
import Simulator.Rules.ReqClos
import Simulator.Rules.SendEnvMsg
import Simulator.Rules.EndCopy

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
  newCallRecord, setCallRecordFn, setCallRecordMissingI, setCallRecordCountI, setCallRecordParam, getCallRecordParam,
  setCallRecordParamIL, setCallRecordParamI, getCallRecordMissing, getCallRecordCount, setCallRecordCount, setCallRecordMissing,
  receiveResponse, receiveUpdate, receiveUpdateClos, receiveUpdateMetaClos, endCopy,
  call, sendEnvMsg, reqEnv, reqClos, assignJob
  ]

