module Simulator.Rules where

import Simulator.Rules.Base
import Simulator.Rules.AssignJob
import Simulator.Rules.Assign
import Simulator.Rules.Return
import Simulator.Rules.If
import Simulator.Rules.Goto
import Simulator.Rules.ReceiveResponse
import Simulator.Rules.ReceiveUpdate
import Simulator.Rules.Resume
import Simulator.Rules.Call
import Simulator.Rules.Wait
import Simulator.Rules.Op
import Simulator.Rules.ReqEnv
import Simulator.Rules.SendEnvMsg
import Simulator.Rules.EndCopy

import Simulator.Rules.NewClosure
import Simulator.Rules.SetClosureFn
import Simulator.Rules.SetClosureMissing
import Simulator.Rules.SetClosureMissingI
import Simulator.Rules.SetClosureCount
import Simulator.Rules.SetClosureCountI
import Simulator.Rules.GetClosureParam
import Simulator.Rules.SetClosureParam
import Simulator.Rules.SetClosureParamI
import Simulator.Rules.SetClosureParamIL
import Simulator.Rules.GetClosureMissing
import Simulator.Rules.GetClosureCount


rules :: [Rule]
rules = [
  assignV, assignL, assignI, returnTerminator, resume, wait, op, ifRule, goto,
  newClosure, setClosureFn, setClosureMissingI, setClosureCountI, setClosureParam, getClosureParam,
  setClosureParamIL, setClosureParamI, getClosureMissing, getClosureCount, setClosureCount, setClosureMissing,
  receiveResponse, receiveUpdate, endCopy,
  call, sendEnvMsg, reqEnv, assignJob
  ]

