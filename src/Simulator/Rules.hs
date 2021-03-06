module Simulator.Rules where

import Simulator.Rules.Base
import Simulator.Rules.AssignJob
import Simulator.Rules.Assign
import Simulator.Rules.Return
import Simulator.Rules.If
import Simulator.Rules.Goto
import Simulator.Rules.Resume
import Simulator.Rules.Call
import Simulator.Rules.CallL
import Simulator.Rules.Wait
import Simulator.Rules.Op
import Simulator.Rules.GetNPU
import Simulator.Rules.InnerCopy
import Simulator.Rules.OuterCopy
import Simulator.Rules.Delete

import Simulator.Rules.Messages.Response
import Simulator.Rules.Messages.Update
import Simulator.Rules.Messages.UpdatePointer
import Simulator.Rules.Messages.UpdateMetaPointer
import Simulator.Rules.Messages.UpdateList
import Simulator.Rules.Messages.UpdateMetaList
import Simulator.Rules.Messages.EndCopy
import Simulator.Rules.Messages.EndReqCopy
import Simulator.Rules.Messages.ReqJobCallRecord
import Simulator.Rules.Messages.ReqPointer
import Simulator.Rules.Messages.SendMsg
import Simulator.Rules.Messages.StepMessage
import Simulator.Rules.StepJob

import Simulator.Rules.List.NewList
import Simulator.Rules.List.NewListN
import Simulator.Rules.List.ListSet
import Simulator.Rules.List.ListGet
import Simulator.Rules.List.ListSetN
import Simulator.Rules.List.ListSetNN
import Simulator.Rules.List.Head
import Simulator.Rules.List.Tail
import Simulator.Rules.List.Last
import Simulator.Rules.List.Length
import Simulator.Rules.List.Concat
import Simulator.Rules.List.Concat3
import Simulator.Rules.List.Map
import Simulator.Rules.List.SMap
import Simulator.Rules.List.Slice
import Simulator.Rules.List.Filter

import Simulator.Rules.CallRecord.NewCallRecord
import Simulator.Rules.CallRecord.SetCallRecordFn
import Simulator.Rules.CallRecord.SetCallRecordMissing
import Simulator.Rules.CallRecord.SetCallRecordMissingI
import Simulator.Rules.CallRecord.SetCallRecordCount
import Simulator.Rules.CallRecord.SetCallRecordCountI
import Simulator.Rules.CallRecord.GetCallRecordParam
import Simulator.Rules.CallRecord.SetCallRecordParam
import Simulator.Rules.CallRecord.SetCallRecordParamI
import Simulator.Rules.CallRecord.SetCallRecordParamIL
import Simulator.Rules.CallRecord.GetCallRecordMissing
import Simulator.Rules.CallRecord.GetCallRecordCount


rules :: [Rule]
rules = [
  assignV, assignL, assignI, returnTerminator, resume, wait, op, getNPU, innerCopy, outerCopy, deleteRule, ifRule, goto,
  newList, newListN, listGet, listSet, listSetN, listSetNN, headRule, tailRule, lastRule, lengthRule, concatRule, concatRule3, mapRule, sMapRule, sliceRule, filterRule,
  newCallRecord, setCallRecordFn, setCallRecordMissingI, setCallRecordCountI, setCallRecordParam, getCallRecordParam,
  setCallRecordParamIL, setCallRecordParamI, getCallRecordMissing, getCallRecordCount, setCallRecordCount, setCallRecordMissing,
  updateMetaPointer, updateMetaList, response, update, updatePointer, updateList, endCopy, endReqCopy,
  sendMsg, reqJobCallRecord, reqPointer, assignJob, call, callL, stepMessage, stepJob
  ]

