module Simulator.Rules.All where

import Simulator.Rules

-- queue rules
import Simulator.Rules.AssignJob
import Simulator.Rules.Call
import Simulator.Rules.NewEnv
import Simulator.Rules.Assign
import Simulator.Rules.Return
import Simulator.Rules.ReceiveResponse
import Simulator.Rules.Resume
import Simulator.Rules.Wait

rules :: [Rule]
rules = [assignJob, assignV, assignL, assignI, envNew, call, returnTerminator, receiveResponse, resume, wait]