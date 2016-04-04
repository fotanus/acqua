module Simulator.Rules where

import Simulator.Rules.Base
import Simulator.Rules.AssignJob
import Simulator.Rules.Call
import Simulator.Rules.EnvNew
import Simulator.Rules.EnvAdd
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

rules :: [Rule]
rules = [
  assignV, assignL, assignI, envNew, returnTerminator, resume, wait, op, ifRule, goto, envAdd,
  receiveResponse, receiveUpdate, endCopy,
  sendEnvMsg, reqEnv, assignJob, call
  ]

