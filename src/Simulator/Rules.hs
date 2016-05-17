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
import Simulator.Rules.Wait
import Simulator.Rules.Op
import Simulator.Rules.ReqEnv
import Simulator.Rules.SendEnvMsg
import Simulator.Rules.EndCopy

import Simulator.Rules.NewClosure
import Simulator.Rules.SetClosureFn
import Simulator.Rules.SetClosureMissingI
import Simulator.Rules.SetClosureCountI
import Simulator.Rules.SetClosureParam


rules :: [Rule]
rules = [
  assignV, assignL, assignI, returnTerminator, resume, wait, op, ifRule, goto,
  newClosure, setClosureFn, setClosureMissingI, setClosureCountI, setClosureParam,
  receiveResponse, receiveUpdate, endCopy,
  sendEnvMsg, reqEnv, assignJob
  ]

